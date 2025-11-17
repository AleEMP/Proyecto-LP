import arcade
import json
import socket
import threading
import struct
import time

# ============================
# CONFIGURACION
# ============================
SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720
SCREEN_TITLE = "Virus (Erlang + Arcade)"

CARD_WIDTH = 80
CARD_HEIGHT = 110
CARD_MARGIN = 10

# Posiciones de los "slots" de órganos
# (Color, (x, y))
MY_BOARD_SLOTS = {
    "red": (300, 200),
    "green": (450, 200),
    "blue": (600, 200),
    "yellow": (750, 200),
    "wild": (900, 200),
}

OPPONENT_BOARD_SLOTS = {
    "red": (300, 500),
    "green": (450, 500),
    "blue": (600, 500),
    "yellow": (750, 500),
    "wild": (900, 500),
}

# Colores para la UI
COLOR_MAP = {
    "red": arcade.color.RADICAL_RED,
    "green": arcade.color.APPLE_GREEN,
    "blue": arcade.color.BLUE_SAPPHIRE,
    "yellow": arcade.color.YELLOW_ORANGE,
    "wild": arcade.color.WHITE,
    "none": arcade.color.LIGHT_GRAY,
}
TYPE_MAP = {
    "organ": arcade.color.BLACK,
    "virus": arcade.color.RADICAL_RED,       # <--- CAMBIADO
    "medicine": arcade.color.BLUE_SAPPHIRE,  # <--- CAMBIADO
    "treatment": arcade.color.PURPLE,        # <--- CAMBIADO
}

# ============================
# CLIENTE DE RED (Sin cambios, tu clase es correcta)
# ============================
class NetworkClient:
    def send(self, data):
        if self.connected:
            try:
                msg = data.encode('utf-8')
                length_prefix = struct.pack('>I', len(msg))
                
                # --- NUEVO LOG DE DEPURACIÓN ---
                print(f"CLIENTE: Enviando {len(msg)} bytes. Prefijo HEX: {length_prefix.hex()}")
                # --- FIN DEL LOG ---
                
                self.sock.sendall(length_prefix + msg)
            except Exception as e:
                print("Error al enviar:", e)
                
    def __init__(self, host, port=4000):
        self.host = host
        self.port = port
        self.sock = None
        self.connected = False
        self.on_message = None

    def connect(self):
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((self.host, self.port))
            self.connected = True
            print(f"Conectado al servidor {self.host}:{self.port}")
            threading.Thread(target=self._listen, daemon=True).start()
        except Exception as e:
            print("ERROR AL CONECTAR:", e)

    def _listen(self):
        while self.connected:
            try:
                prefix_data = self.sock.recv(4)
                if not prefix_data:
                    break
                msg_len = struct.unpack('>I', prefix_data)[0]
                data = b""
                while len(data) < msg_len:
                    chunk = self.sock.recv(msg_len - len(data))
                    if not chunk:
                        raise Exception("Socket cerrado durante lectura")
                    data += chunk
                
                msg = data.decode("utf-8").strip()
                
                if self.on_message and msg:
                    # El planificador de Arcade es necesario
                    # para actualizar el estado del juego desde el hilo de red.
                    # Usamos una función interna para llamarla una sola vez.
                    
                    def single_shot_callback(delta_time):
                        # Llama al procesador de mensajes
                        self.on_message(delta_time, msg)
                        # Se des-agenda a sí misma para no volver a ejecutarse
                        arcade.unschedule(single_shot_callback)

                    # Agendamos la función para que se ejecute en el próximo "tick"
                    arcade.schedule(single_shot_callback, 0)
            except Exception as e:
                print(f"Error en _listen: {e}")
                break
        self.connected = False
        print("Conexión cerrada con el servidor.")

# ============================
# CLIENTE GRAFICO (Modificado para VIRUS)
# ============================
class VirusClient(arcade.Window):
    def __init__(self, server_ip):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_TITLE)
        arcade.set_background_color(arcade.color.DARK_BLUE_GRAY)

        # Red
        self.network = NetworkClient(server_ip)
        self.network.on_message = self._process_server_message
        self.network.connect()

        # --- Estado del Juego (local) ---
        self.my_pid = None
        self.is_my_turn = False
        self.status_text = "Conectando..."
        
        self.game_state = {
            "hands": {},
            "boards": {}
        }
        
        # UI State
        self.player_hand_sprites = arcade.SpriteList()
        self.selected_card_index = None # <---
        self.selected_card_data = None
        
        # <--- NUEVO: Variable de estado para la jugada de Trasplante ---
        self.transplant_source_color = None # Almacena el color de nuestro órgano

        # Botón de Start
        self.start_button_list = arcade.SpriteList()
        self.start_button = arcade.SpriteSolidColor(200, 50, arcade.color.GREEN)
        self.start_button.center_x = 100
        self.start_button.center_y = SCREEN_HEIGHT - 50
        self.start_button_list.append(self.start_button)

    def get_my_hand(self):
        if not self.my_pid: return []
        return self.game_state.get("hands", {}).get(self.my_pid, [])
    
    def get_my_board(self):
        if not self.my_pid: return {}
        return self.game_state.get("boards", {}).get(self.my_pid, {})
        
    def get_opponent_pid_and_board(self):
        if not self.my_pid: return None, {}
        for pid, board in self.game_state.get("boards", {}).items():
            if pid != self.my_pid:
                return pid, board
        return None, {}

    # ============================
    # PROCESAMIENTO DE MENSAJES
    # ============================
    def _process_server_message(self, delta_time, message):
            print(f"Servidor -> {message}")
            try:
                data = json.loads(message)
                action = data.get("action")
                
                if action == "welcome":
                    self.my_pid = data.get("my_pid")
                    self.status_text = f"Conectado! Soy {self.my_pid}"

                # Estos mensajes ya no se usan (se reemplazan por update_state)
                # elif action == "your_turn":
                # elif action == "draw_card":

                elif action == "play_error":
                    self.status_text = f"Error: {data.get('reason')}"
                    # No hay que devolver la carta, solo resetear la selección
                    self.selected_card_index = None # <---
                    self.selected_card_data = None
                    self.transplant_source_color = None

                elif action == "play_ok":
                    self.status_text = "Jugada aceptada."
                    # La jugada fue buena. 'send_play_card' ya reseteó el estado.
                    # Esperamos el 'update_state' para confirmar.
                    pass

                elif action == "update_state":
                    self.game_state = data.get("state")
                    self.status_text = "Estado actualizado."
                    self.is_my_turn = (self.game_state.get("current_player") == self.my_pid)
                    self.update_hand_sprites()

                    # El servidor nos envía el estado. Si el "game_stage"
                    # ya NO es "waiting_for_players", significa que el
                    # juego ha comenzado y debemos ocultar el botón.
                    game_stage = self.game_state.get("game_stage")
                    if game_stage and game_stage != "waiting_for_players":
                        self.start_button_list.clear()

            except Exception as e:
                print("Error al interpretar JSON:", e)       

    def update_hand_sprites(self):
        self.player_hand_sprites.clear()
        my_hand = self.get_my_hand()
        
        for i, card_data in enumerate(my_hand):
            x = 100 + i * (CARD_WIDTH + CARD_MARGIN)
            y = 70
            
            card_sprite = arcade.SpriteSolidColor(CARD_WIDTH, CARD_HEIGHT, arcade.color.WHITE)
            card_sprite.center_x = x
            card_sprite.center_y = y
            card_sprite.data = card_data # Guardamos los datos
            card_sprite.index = i # Guardamos el índice
            
            self.player_hand_sprites.append(card_sprite)

    # ============================
    # EVENTOS GRAFICOS
    # ============================

    def on_draw(self):
        self.clear()
        
        # Dibujar mano e info (Sin cambios)
        self.player_hand_sprites.draw()
        for i, sprite in enumerate(self.player_hand_sprites):
                card = sprite.data
                arcade.draw_text(card['name'], sprite.center_x, sprite.center_y + 10, TYPE_MAP[card['type']], 10, align="center", anchor_x="center")
                arcade.draw_text(card['type'], sprite.center_x, sprite.center_y - 10, TYPE_MAP[card['type']], 10, align="center", anchor_x="center")
                
                # --- NUEVA LÓGICA DE DIBUJO ---
                # Resaltar la carta seleccionada
                if i == self.selected_card_index:
                    arcade.draw_lrbt_rectangle_outline(
                        left=sprite.center_x - CARD_WIDTH / 2 - 2,
                        right=sprite.center_x + CARD_WIDTH / 2 + 2,
                        bottom=sprite.center_y - CARD_HEIGHT / 2 - 2,
                        top=sprite.center_y + CARD_HEIGHT / 2 + 2,
                        color=arcade.color.YELLOW,
                        border_width=4
                    )
                else:
                    # Dibujar el borde normal
                    arcade.draw_lrbt_rectangle_outline(
                        left=sprite.center_x - CARD_WIDTH / 2,
                        right=sprite.center_x + CARD_WIDTH / 2,
                        bottom=sprite.center_y - CARD_HEIGHT / 2,
                        top=sprite.center_y + CARD_HEIGHT / 2,
                        color=COLOR_MAP[card['color']],
                        border_width=2
                    )
        # Dibujar estado
        turno_text = "¡Mi Turno!" if self.is_my_turn else "Turno del Oponente"
        arcade.draw_text(f"Estado: {turno_text}", 20, SCREEN_HEIGHT - 70, arcade.color.WHITE, 18)
        arcade.draw_text(self.status_text, 20, SCREEN_HEIGHT - 100, arcade.color.CYAN, 16)
        
        # <--- MODIFICADO: Mostrar estado de Trasplante ---
        if self.selected_card_data:
            if self.transplant_source_color:
                # Muestra la carta Y el órgano de origen ya seleccionado
                arcade.draw_text(f"Seleccionado: {self.selected_card_data['name']} (Origen: {self.transplant_source_color})", 
                                 20, 20, arcade.color.YELLOW, 16)
            else:
                # Muestra solo la carta seleccionada
                arcade.draw_text(f"Seleccionado: {self.selected_card_data['name']}", 
                                 20, 20, arcade.color.YELLOW, 16)

        # Dibujar botón de Start
        self.start_button_list.draw()
        arcade.draw_text("START GAME", self.start_button.center_x, self.start_button.center_y, arcade.color.BLACK, 14, anchor_x="center", anchor_y="center")

        # Dibujar Tableros (Sin cambios)
        self.draw_board(self.get_my_board(), MY_BOARD_SLOTS, "Mi Tablero")
        opp_pid, opp_board = self.get_opponent_pid_and_board()
        if opp_pid:
            self.draw_board(opp_board, OPPONENT_BOARD_SLOTS, f"Tablero Oponente ({opp_pid[:10]}...)")

    def draw_board(self, board_data, slot_positions, title):
        arcade.draw_text(title, slot_positions["red"][0] - 100, slot_positions["red"][1] + 80, arcade.color.WHITE, 14)
        
        for color, (x, y) in slot_positions.items():
            slot_data = board_data.get(color, {"state": 0, "cards": []})
            state = slot_data.get("state", 0)
            cards_in_slot = slot_data.get("cards", [])
            
            # Dibujar el slot
            slot_color = COLOR_MAP[color]
            
            # arcade.draw_rectangle_outline ya no existe, usamos draw_lrtb_rectangle_outline
            # Convertimos de (centro, ancho) a (izquierda, derecha, arriba, abajo)
            width = CARD_WIDTH + 10
            height = CARD_HEIGHT + 10
            arcade.draw_lrbt_rectangle_outline(  # <--- CORREGIDO a 'lrbt'
                left=x - width / 2,
                right=x + width / 2,
                bottom=y - height / 2,  # <--- 'bottom' va primero
                top=y + height / 2,      # <--- 'top' va después
                color=slot_color,
                border_width=2
            )
            
            arcade.draw_text(color, x, y + 70, slot_color, 12, anchor_x="center")

            # Dibujar estado del slot (simplificado)
            if state == 0:
                arcade.draw_text("VACIO", x, y, arcade.color.GRAY, 12, anchor_x="center")
            else:
                # Dibujar la carta superior (simplificado)
                top_card_name = cards_in_slot[0].get("name", "CARTA") if cards_in_slot else "???"
                top_card_type = cards_in_slot[0].get("type", "organ") if cards_in_slot else "organ"
                arcade.draw_rectangle_filled(x, y, CARD_WIDTH, CARD_HEIGHT, TYPE_MAP[top_card_type])
                arcade.draw_text(top_card_name, x, y, arcade.color.WHITE, 10, anchor_x="center")
                
            arcade.draw_text(f"Estado: {state}", x, y - 70, arcade.color.WHITE, 12, anchor_x="center")


    # ============================
    # LÓGICA DE CLICS (REESTRUCTURADA)
    # ============================

    # <--- NUEVO: Helper para detectar clics en tableros ---
    def get_board_click(self, x, y, slot_map, owner_pid):
        """
        Comprueba si (x,y) está en un slot del mapa.
        Retorna: {'pid': pid, 'color': color} o None.
        """
        for color, (cx, cy) in slot_map.items():
            if (x > cx - CARD_WIDTH/2 and x < cx + CARD_WIDTH/2 and
                y > cy - CARD_HEIGHT/2 and y < cy + CARD_HEIGHT/2):
                return {"pid": owner_pid, "color": color}
        return None

    def on_mouse_press(self, x, y, button, modifiers):
        clicked_buttons = arcade.get_sprites_at_point((x, y), self.start_button_list)
        if clicked_buttons:
            print("Enviando START GAME...")
            self.network.send(json.dumps({"action": "start_game"}))
            return

        # 2. Si no fue en el botón de Start, AHORA sí chequeamos si es tu turno
        if not self.is_my_turn:
            self.status_text = "No es tu turno."
            return
            
        # 3. Chequear clic en una Carta de la Mano
        cards_clicked = arcade.get_sprites_at_point((x, y), self.player_hand_sprites)
        if cards_clicked:
            clicked_card_sprite = cards_clicked[0]

            # Si hacemos clic en la carta que YA está seleccionada, la "soltamos"
            if self.selected_card_data and self.selected_card_index == clicked_card_sprite.index:
                self.selected_card_index = None
                self.selected_card_data = None
                self.transplant_source_color = None
                self.status_text = "Selección cancelada."
                return

            # Seleccionamos la nueva carta (o cambiamos la selección)
            self.selected_card_index = clicked_card_sprite.index
            self.selected_card_data = clicked_card_sprite.data
            
            self.transplant_source_color = None 
            
            if self.selected_card_data['name'] == 'transplant':
                self.status_text = f"Trasplante: Elige TU órgano de origen."
            else:
                self.status_text = f"Carta {self.selected_card_data['name']} seleccionada. Elige un objetivo."
            
            # NO quitamos la carta de la mano. Solo actualizamos la UI.
            return

        # 4. Si ya hay una carta seleccionada, chequear clic en un Objetivo (Slot)
        if self.selected_card_data:
            
            # --- Identificar dónde se hizo clic ---
            my_board_click = self.get_board_click(x, y, MY_BOARD_SLOTS, self.my_pid)
            
            opp_board_click = None
            opp_pid, _ = self.get_opponent_pid_and_board()
            if opp_pid:
                opp_board_click = self.get_board_click(x, y, OPPONENT_BOARD_SLOTS, opp_pid)

            click_target = my_board_click or opp_board_click

            # --- CASO A: LÓGICA DE TRASPLANTE (2 pasos) ---
            if self.selected_card_data['name'] == 'transplant':
                self.handle_transplant_click(my_board_click, opp_board_click)
            
            # --- CASO B: LÓGICA NORMAL (1 paso) ---
            else:
                self.handle_normal_click(click_target)


    # <--- NUEVO: Helper para lógica de Trasplante ---
    def handle_transplant_click(self, my_board_click, opp_board_click):
        """Maneja la lógica de clics de 2 pasos para Trasplante."""
        
        # Estado 1: Esperando el primer clic (en nuestro tablero)
        if self.transplant_source_color is None:
            if my_board_click:
                self.transplant_source_color = my_board_click['color']
                self.status_text = f"Origen '{self.transplant_source_color}' OK. Ahora elige el órgano del OPONENTE."
            else:
                self.status_text = "Inválido. Debes elegir TU PROPIO órgano primero."
        
        # Estado 2: Esperando el segundo clic (en tablero oponente)
        else:
            if opp_board_click:
                target_pid = opp_board_click['pid']
                target_color = opp_board_click['color']
                
                # ¡Tenemos todo! Enviar la jugada.
                self.send_play_card(
                    target_pid, 
                    target_color, 
                    player_color=self.transplant_source_color # <--- Pasamos el 3er argumento
                )
            else:
                self.status_text = "Inválido. Debes elegir el órgano del OPONENTE."


    # <--- NUEVO: Helper para lógica normal ---
    def handle_normal_click(self, click_target):
        """Maneja la lógica de clic estándar de 1 paso."""
        if click_target:
            target_pid = click_target['pid']
            target_color = click_target['color']
            self.send_play_card(target_pid, target_color)
        else:
            self.status_text = "Clic en un slot de órgano válido (tuyo o del oponente)."

    # <--- MODIFICADO: Acepta 'player_color' y resetea el estado ---
    def send_play_card(self, target_pid, target_color, player_color=None):
        card = self.selected_card_data
        
        player_color_for_api = player_color if player_color else "none"

        msg = {
            "action": "play_card",
            "target_pid": target_pid,
            "card": {
                "type": card.get("type"),
                "color": card.get("color"),
                "name": card.get("name")
            },
            "player_color": player_color_for_api, 
            "target_color": target_color 
        }
        
        print(f"Enviando jugada: {msg}")
        self.network.send(json.dumps(msg))

        # --- BLOQUE DE QUITAR CARTA ELIMINADO ---
        # Ya no quitamos la carta aquí.
        # Esperamos a que el servidor nos envíe 'update_state'.
        
        # --- RESETEAR ESTADO DE SELECCIÓN (SOLAMENTE) ---
        self.selected_card_index = None
        self.selected_card_data = None
        self.transplant_source_color = None
# ============================
# MAIN
# ============================

def main():
    server_ip = input("IP del servidor Erlang (ej. 127.0.0.1): ").strip()
    if not server_ip:
        server_ip = "127.0.0.1"
    
    game = VirusClient(server_ip)
    arcade.run()

if __name__ == "__main__":
    main()