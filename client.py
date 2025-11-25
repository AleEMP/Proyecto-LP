import arcade
import json
import socket
import threading
import struct
import time
from pathlib import Path 

SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720
SCREEN_TITLE = "Virus (Erlang + Arcade)"

CARD_WIDTH = 80
CARD_HEIGHT = 110
CARD_MARGIN = 10

IMAGE_PATH = Path(__file__).parent / "imagenes"


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
DISCARD_PILE_SLOT = (1100, 350)

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
    "virus": arcade.color.RADICAL_RED,
    "medicine": arcade.color.BLUE_SAPPHIRE,
    "treatment": arcade.color.PURPLE,
}

class NetworkClient:
    def send(self, data):
        if self.connected:
            try:
                msg = data.encode('utf-8')
                length_prefix = struct.pack('>I', len(msg))
                print(f"CLIENTE: Enviando {len(msg)} bytes. Prefijo HEX: {length_prefix.hex()}")
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
                    def single_shot_callback(delta_time):
                        self.on_message(delta_time, msg)
                        arcade.unschedule(single_shot_callback)
                    arcade.schedule(single_shot_callback, 0)
            except Exception as e:
                print(f"Error en _listen: {e}")
                break
        self.connected = False
        print("Conexión cerrada con el servidor.")

class VirusClient(arcade.Window):
    def __init__(self, server_ip):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_TITLE)
        arcade.set_background_color(arcade.color.DARK_BLUE_GRAY)
        #arcade.set_background_color(arcade.color.BARBIE_PINK)

        self.network = NetworkClient(server_ip)
        self.network.on_message = self._process_server_message
        self.network.connect()

        self.my_pid = None
        self.is_my_turn = False
        self.status_text = "Conectando..."
        self.game_state = {"hands": {}, "boards": {}}
        
        self.player_hand_sprites = arcade.SpriteList()
        
        self.selected_card_indices = []
        self.selected_card_data = None
        self.selected_card_index = None

        self.is_discarding = False
        self.contagion_mode = False
        self.contagion_source_color = None
        self.transplant_source_color = None

        self.card_textures = {}
        self.load_all_textures()

        self.start_button_list = arcade.SpriteList()
        self.start_button = arcade.SpriteSolidColor(200, 50, arcade.color.GREEN)
        self.start_button.center_x = 100
        self.start_button.center_y = SCREEN_HEIGHT - 50
        self.start_button_list.append(self.start_button)


    def load_all_textures(self):
        
        print("--- INICIANDO CLIENTE CON TEXTURAS (Versión 3.0) ---")
        print(f"Buscando imágenes en: {IMAGE_PATH.resolve()}")
        
        file_types = ("*.png", "*.jpg", "*.jpeg")
        image_files = []
        for file_type in file_types:
            image_files.extend(IMAGE_PATH.glob(file_type))
        
        if not image_files:
            print(">>> ¡ALERTA! No se encontraron archivos .png o .jpg en la carpeta 'imagenes'.")
            print(">>> Asegúrate de que la carpeta 'imagenes' esté al mismo nivel que 'client.py'.")

        for img_file in image_files:
            try:
                texture = arcade.load_texture(img_file)
                self.card_textures[img_file.stem] = texture
            except Exception as e:
                print(f"Error al cargar {img_file}: {e}")
        
        print(f"Texturas cargadas con éxito: {list(self.card_textures.keys())}")
        print("-----------------------------------------------------")

    def get_card_texture(self, card_data):

        card_name = card_data.get("name")
        card_type = card_data.get("type")
        card_color = card_data.get("color")
       
        special_names = {
            "contagion": "contaguos_card", 
            "latex_glove": "latexglove_card",
            "medical_mistake": "medicmistake_card",
            "organ_thief": "thief_card",
            "transplant": "transplant_card"
        }

        if card_name in special_names:
            texture_name = special_names[card_name]
        
        elif card_type in ["organ", "virus", "medicine"]:
            texture_name = f"{card_type}_{card_color}"
        
        else:
            texture_name = f"{card_type}_{card_color}"

        if texture_name not in self.card_textures and card_type == "organ":
             texture_name = f"organ_{card_color}"

        texture = self.card_textures.get(texture_name)
        if not texture:
            print(f"ALERTA: No se encontró textura en caché para: {texture_name} (Datos: {card_data})")
        return texture

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

    def _process_server_message(self, delta_time, message):
            print(f"Servidor -> {message}")
            try:
                data = json.loads(message)
                action = data.get("action")
                
                print(f"Action: {action}, Data completa: {data}")
                
                if action == "welcome":
                    self.my_pid = data.get("my_pid")
                    self.status_text = f"Conectado! Soy {self.my_pid}"

                elif action == "play_error":
                    self.status_text = f"Error: {data.get('reason')}"
                    self.selected_card_index = None
                    self.selected_card_data = None
                    self.transplant_source_color = None

                elif action == "play_ok":
                    #arcade.set_background_color(arcade.color.FALU_RED)
                    print(f"Play_ok data: {data}")
                    self.status_text = "Jugada aceptada."
                    pass

                elif action == "contagion_start":
                    arcade.set_background_color(arcade.color.SEA_GREEN)
                    self.contagion_mode = True
                    self.contagion_source_color = None
                    self.status_text = "Modo Contagio activo. Elige órgano de origen."
                
                elif action == "contagion_end":
                    arcade.set_background_color(arcade.color.DARK_BLUE_GRAY)
                    self.contagion_mode = False
                    self.contagion_source_color = None
                    self.status_text = "Modo Contagio terminado."

                elif action == "contagion_ok":
                    self.status_text = "¡Contagio exitoso! Puedes seguir contagiando."

                elif action == "contagion_error":
                    self.status_text = f"Error en contagio: {data.get('reason')}"
                    self.contagion_source_color = None
                    self.selected_card_index = None
                    self.selected_card_data = None

                elif action == "update_state":
                    self.game_state = data.get("state")
                    self.status_text = "Estado actualizado."
                    self.is_my_turn = (self.game_state.get("current_player") == self.my_pid)
                    self.update_hand_sprites()

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
            
            texture = self.get_card_texture(card_data)
            
            if texture:
               
                card_sprite = arcade.Sprite()
                card_sprite.texture = texture

                card_sprite.width = CARD_WIDTH
                card_sprite.height = CARD_HEIGHT
            else:
                card_sprite = arcade.SpriteSolidColor(CARD_WIDTH, CARD_HEIGHT, arcade.color.MAGENTA)

            card_sprite.center_x = x
            card_sprite.center_y = y
            card_sprite.data = card_data
            card_sprite.index = i
            
            self.player_hand_sprites.append(card_sprite)

    def on_draw(self):
        self.clear()
        
        self.player_hand_sprites.draw()
        
        for i, sprite in enumerate(self.player_hand_sprites):
            border_color = arcade.color.YELLOW
            border_width = 4
            
            if i in self.selected_card_indices:
                pass 
            elif i == self.selected_card_index:
                pass 
            else:
                continue 

            arcade.draw_lrbt_rectangle_outline(
                left=sprite.center_x - sprite.width / 2 - 2,
                right=sprite.center_x + sprite.width / 2 + 2,
                bottom=sprite.center_y - sprite.height / 2 - 2,
                top=sprite.center_y + sprite.height / 2 + 2,
                color=border_color,
                border_width=border_width
            )

        turno_text = "¡Mi Turno!" if self.is_my_turn else "Turno del Oponente"
        arcade.draw_text(f"Estado: {turno_text}", 20, SCREEN_HEIGHT - 70, arcade.color.WHITE, 18)
        arcade.draw_text(self.status_text, 20, SCREEN_HEIGHT - 100, arcade.color.CYAN, 16)
        
        if self.selected_card_data:
            if self.transplant_source_color:
                arcade.draw_text(f"Seleccionado: {self.selected_card_data['name']} (Origen: {self.transplant_source_color})", 
                                 20, 20, arcade.color.YELLOW, 16)
            else:
                arcade.draw_text(f"Seleccionado: {self.selected_card_data['name']}", 
                                 20, 20, arcade.color.YELLOW, 16)

        self.start_button_list.draw()
        arcade.draw_text("START GAME", self.start_button.center_x, self.start_button.center_y, arcade.color.BLACK, 14, anchor_x="center", anchor_y="center")

        self.draw_board(self.get_my_board(), MY_BOARD_SLOTS, "Mi Tablero")
        opp_pid, opp_board = self.get_opponent_pid_and_board()
        if opp_pid:
            self.draw_board(opp_board, OPPONENT_BOARD_SLOTS, f"Tablero Oponente ({opp_pid[:10]}...)")
        
        
        dp_x, dp_y = DISCARD_PILE_SLOT
        arcade.draw_text("Descartar", dp_x, dp_y + 70, arcade.color.WHITE, 12, anchor_x="center")
        
        top_card = self.game_state.get("top_discard_card")
        
        if top_card and top_card != 'null':
            texture = self.get_card_texture(top_card)
            if texture:
               
                temp_list = arcade.SpriteList()
                temp_sprite = arcade.Sprite()
                temp_sprite.texture = texture
                temp_sprite.center_x = dp_x
                temp_sprite.center_y = dp_y
                temp_sprite.width = CARD_WIDTH
                temp_sprite.height = CARD_HEIGHT
                temp_list.append(temp_sprite) 
                temp_list.draw()
        else:
            discard_pile_color = arcade.color.YELLOW if self.is_discarding else arcade.color.GRAY
            arcade.draw_lrbt_rectangle_outline(
                left=dp_x - CARD_WIDTH / 2, right=dp_x + CARD_WIDTH / 2,
                bottom=dp_y - CARD_HEIGHT / 2, top=dp_y + CARD_HEIGHT / 2,
                color=discard_pile_color, border_width=3
            )
        
        if self.is_discarding and len(self.selected_card_indices) > 0:
            arcade.draw_text(f"{len(self.selected_card_indices)} / 3", dp_x, dp_y - 70, arcade.color.YELLOW, 12, anchor_x="center")


    def draw_board(self, board_data, slot_positions, title):
        arcade.draw_text(title, slot_positions["red"][0] - 100, slot_positions["red"][1] + 80, arcade.color.WHITE, 14)
        
        for color, (x, y) in slot_positions.items():
            slot_data = board_data.get(color, {"state": 0, "cards": []})
            state = slot_data.get("state", 0)
            cards_in_slot = slot_data.get("cards", [])
            
            slot_color = COLOR_MAP[color]
            arcade.draw_lrbt_rectangle_outline(
                left=x - CARD_WIDTH / 2, right=x + CARD_WIDTH / 2,
                bottom=y - CARD_HEIGHT / 2, top=y + CARD_HEIGHT / 2,
                color=slot_color, border_width=2
            )
            arcade.draw_text(color, x, y + 70, slot_color, 12, anchor_x="center")

            if state == 0:
                arcade.draw_text("VACIO", x, y, arcade.color.GRAY, 12, anchor_x="center")
            else:
                cards_to_draw = list(reversed(cards_in_slot))
                STACK_OFFSET = 20 
                stack_height = (len(cards_to_draw) - 1) * STACK_OFFSET
                base_y = y - stack_height / 2
                y_offset = 0
                
                for card in cards_to_draw:
                    card_y = base_y + y_offset
                    texture = self.get_card_texture(card) 
                    
                    if texture:
                        temp_list = arcade.SpriteList() 
                        temp_sprite = arcade.Sprite()
                        temp_sprite.texture = texture
                        temp_sprite.center_x = x
                        temp_sprite.center_y = card_y
                        temp_sprite.width = CARD_WIDTH
                        temp_sprite.height = CARD_HEIGHT
                        temp_list.append(temp_sprite) 
                        temp_list.draw() 
                    y_offset += STACK_OFFSET
                
            arcade.draw_text(f"Estado: {state}", x, y - 70, arcade.color.WHITE, 12, anchor_x="center")


    def get_board_click(self, x, y, slot_map, owner_pid):
        for color, (cx, cy) in slot_map.items():
            if (x > cx - CARD_WIDTH/2 and x < cx + CARD_WIDTH/2 and
                y > cy - CARD_HEIGHT/2 and y < cy + CARD_HEIGHT/2):
                return {"pid": owner_pid, "color": color}
        return None

    def on_mouse_press(self, x, y, button, modifiers):
        if self.contagion_mode:
            self.status_text = "Modo Contagio activo."
            self.handle_contagion_mode_click(x, y)
            return
        else:

            if button == arcade.MOUSE_BUTTON_RIGHT and self.is_discarding:
                self.is_discarding = False
                self.selected_card_indices = []
                self.status_text = "Modo descarte cancelado."
                return
        
            clicked_buttons = arcade.get_sprites_at_point((x, y), self.start_button_list)
            if clicked_buttons:
                print("Enviando START GAME...")
                self.network.send(json.dumps({"action": "start_game"}))
                return

            if not self.is_my_turn:
                self.status_text = "No es tu turno."
                return
                
            dp_x, dp_y = DISCARD_PILE_SLOT
            if (x > dp_x - CARD_WIDTH/2 and x < dp_x + CARD_WIDTH/2 and
                y > dp_y - CARD_HEIGHT/2 and y < dp_y + CARD_HEIGHT/2):
                
                if self.is_discarding:
                    self.send_discard_cards()
                else:
                    self.is_discarding = True
                    self.selected_card_index = None
                    self.selected_card_data = None
                    self.selected_card_indices = []
                    self.status_text = "MODO DESCARTE: Selecciona hasta 3 cartas. Clic en el mazo de descarte para confirmar."
                return

            cards_clicked = arcade.get_sprites_at_point((x, y), self.player_hand_sprites)
            if cards_clicked:
                clicked_card_sprite = cards_clicked[0]
                idx = clicked_card_sprite.index
                
                if self.is_discarding:
                    if idx in self.selected_card_indices:
                        self.selected_card_indices.remove(idx)
                    else:
                        if len(self.selected_card_indices) < 3:
                            self.selected_card_indices.append(idx)
                        else:
                            self.status_text = "Límite de 3 cartas para descarte."
                    return
                    
                else:
                    if self.selected_card_data and self.selected_card_index == idx:
                        self.selected_card_index = None
                        self.selected_card_data = None
                        self.status_text = "Selección cancelada."
                    else:
                        self.selected_card_index = idx
                        self.selected_card_data = clicked_card_sprite.data
                        self.transplant_source_color = None 
                        if self.selected_card_data['name'] == 'transplant':
                            self.status_text = f"Trasplante: Elige tu órgano de origen."
                        else:
                            self.status_text = f"Carta {self.selected_card_data['name']} seleccionada. Elige un objetivo."
                    return

            if self.selected_card_data and not self.is_discarding:
                my_board_click = self.get_board_click(x, y, MY_BOARD_SLOTS, self.my_pid)
                
                opp_board_click = None
                opp_pid, _ = self.get_opponent_pid_and_board()
                if opp_pid:
                    opp_board_click = self.get_board_click(x, y, OPPONENT_BOARD_SLOTS, opp_pid)

                click_target = my_board_click or opp_board_click

                if self.selected_card_data['name'] == 'transplant':
                    self.handle_transplant_click(my_board_click, opp_board_click)
                else:
                    self.handle_normal_click(click_target)

    def handle_contagion_mode_click(self, x, y):
        cards_clicked = arcade.get_sprites_at_point((x, y), self.player_hand_sprites)
        if cards_clicked:
            self.status_text = "En modo contagio: selecciona órganos del tablero"
            return
        
        my_board_click = self.get_board_click(x, y, MY_BOARD_SLOTS, self.my_pid)
        opp_pid, _ = self.get_opponent_pid_and_board()
        opp_board_click = self.get_board_click(x, y, OPPONENT_BOARD_SLOTS, opp_pid) if opp_pid else None
        
        if self.contagion_source_color is None:
            if my_board_click:
                self.contagion_source_color = my_board_click['color']
                self.status_text = f"Origen '{self.contagion_source_color}' OK. Ahora elige objetivo del OPONENTE."
            else:
                self.status_text = "Inválido. Debes elegir TU PROPIO órgano infectado primero."
        else:
            if opp_board_click:
                target_pid = opp_board_click['pid']
                target_color = opp_board_click['color']
                self.send_play_contagion(
                    target_pid,
                    self.contagion_source_color, 
                    target_color
                )
            else:
                self.status_text = "Inválido. Debes elegir el órgano del OPONENTE."

    def handle_transplant_click(self, my_board_click, opp_board_click):
        if self.transplant_source_color is None:
            if my_board_click:
                self.transplant_source_color = my_board_click['color']
                self.status_text = f"Origen '{self.transplant_source_color}' OK. Ahora elige el órgano del OPONENTE."
            else:
                self.status_text = "Inválido. Debes elegir TU PROPIO órgano primero."
        else:
            if opp_board_click:
                target_pid = opp_board_click['pid']
                target_color = opp_board_click['color']
                self.send_play_card(
                    target_pid, 
                    target_color, 
                    player_color=self.transplant_source_color
                )
            else:
                self.status_text = "Inválido. Debes elegir el órgano del OPONENTE."

    def handle_normal_click(self, click_target):
        if click_target:
            target_pid = click_target['pid']
            target_color = click_target['color']
            self.send_play_card(target_pid, target_color)
        else:
            self.status_text = "Clic en un slot de órgano válido (tuyo o del oponente)."

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
        
        self.selected_card_index = None
        self.selected_card_data = None
        self.transplant_source_color = None

    def send_play_contagion(self, target_pid, player_color, target_color):
        msg = {
            "action": "play_contagion",
            "target_pid": target_pid,
            "player_color": player_color,
            "target_color": target_color
        }
        self.network.send(json.dumps(msg))
        
        #self.contagion_mode = False
        self.contagion_source_color = None

    def send_discard_cards(self):
        
        if not self.selected_card_indices:
            self.status_text = "No hay cartas seleccionadas para descartar."
            return

        cards_to_discard = []
        my_hand = self.get_my_hand() 
        for i in self.selected_card_indices:
            try:
                cards_to_discard.append(my_hand[i]) 
            except IndexError:
                print(f"Error: Índice de descarte {i} fuera de rango para mano de tamaño {len(my_hand)}")

        msg = {
            "action": "discard_cards",
            "cards": cards_to_discard
        }

        print(f"Enviando descarte: {len(cards_to_discard)} cartas")
        self.network.send(json.dumps(msg))

        self.selected_card_indices = []
        self.is_discarding = False

def main():
    server_ip = input("IP del servidor Erlang (ej. 127.0.0.1): ").strip()
    if not server_ip:
        server_ip = "127.0.0.1"
    
    game = VirusClient(server_ip)
    arcade.run()

if __name__ == "__main__":
    main()