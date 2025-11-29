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

IMAGE_PATH = Path(__file__).parent / "imagenes"

COLOR_MAP = {
    "red": arcade.color.RADICAL_RED,
    "green": arcade.color.APPLE_GREEN,
    "blue": arcade.color.BLUE_SAPPHIRE,
    "yellow": arcade.color.YELLOW_ORANGE,
    "wild": arcade.color.WHITE,
    "none": arcade.color.LIGHT_GRAY,
}

class NetworkClient:
    def send(self, data):
        if self.connected:
            try:
                msg = data.encode('utf-8')
                length_prefix = struct.pack('>I', len(msg))
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
    def __init__(self, nickname):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_TITLE, resizable=True)
        arcade.set_background_color(arcade.color.DARK_BLUE_GRAY)

        self.nickname = nickname
        self.network = None
        self.server_ip = "127.0.0.1" 

        self.my_pid = None
        self.is_my_turn = False
        self.status_text = "Conectando..."
        self.game_state = {"hands": {}, "boards": {}, "players": [], "nicknames": {}}
        self.winner_pid = None
        
        
        self.card_width = 60
        self.card_height = 85
        self.card_margin = 5
        self.row_height = 100
        self.start_y = 180
        self.start_x = 350
        self.slot_spacing = 80
        
        
        self.font_small = 10
        self.font_normal = 12
        self.font_medium = 14
        self.font_large = 18
        self.font_huge = 54  
        self.font_icon = 80  
        
        self.player_hand_sprites = arcade.SpriteList()
        self.selected_card_indices = []
        self.selected_card_data = None
        self.selected_card_index = None

        self.is_discarding = False
        self.transplant_source_color = None
        self.is_contagion_active = False
        self.contagion_source_color = None
        self.source_is_wild = False 

        self.card_textures = {}
        self.load_all_textures()

        self.start_button_list = arcade.SpriteList()
        self.start_button = arcade.SpriteSolidColor(200, 50, arcade.color.GREEN)
        self.start_button.center_x = 100
        self.start_button.center_y = SCREEN_HEIGHT - 50
        self.start_button_list.append(self.start_button)


    @property
    def discard_pile_pos(self):
        return (self.width * 0.9, self.height * 0.5)

    def connect_and_join(self):
        self.network = NetworkClient(self.server_ip)
        self.network.on_message = self._process_server_message
        self.network.connect()
        time.sleep(0.1)
        msg = json.dumps({"action": "join", "nickname": self.nickname})
        self.network.send(msg)

    def load_all_textures(self):
        print("--- INICIANDO CLIENTE ---")
        file_types = ("*.png", "*.jpg", "*.jpeg")
        image_files = []
        for file_type in file_types:
            image_files.extend(IMAGE_PATH.glob(file_type))
        if not image_files:
            print(">>> ¡ALERTA! No se encontraron imágenes.")
        for img_file in image_files:
            try:
                texture = arcade.load_texture(img_file)
                self.card_textures[img_file.stem] = texture
            except Exception as e:
                print(f"Error al cargar {img_file}: {e}")

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

        return self.card_textures.get(texture_name)

    def get_my_hand(self):
        if not self.my_pid: return []
        return self.game_state.get("hands", {}).get(self.my_pid, [])
    
    def on_resize(self, width, height):
        super().on_resize(width, height)
        self.start_button.center_x = width * 0.1
        self.start_button.center_y = height * 0.1
        self.recalculate_layout()
        self.update_hand_sprites()


    def recalculate_layout(self):
        num_players = len(self.game_state.get("players", []))
        if num_players == 0: num_players = 1


        scale_factor = self.height / 720.0
        
        
        self.font_small = int(10 * scale_factor)
        self.font_normal = int(12 * scale_factor)
        self.font_medium = int(14 * scale_factor)
        self.font_large = int(18 * scale_factor)
        self.font_huge = int(54 * scale_factor)
        self.font_icon = int(80 * scale_factor)


        if num_players <= 2:
            pct_height = 0.20 
            self.card_height = self.height * pct_height
            self.row_height = self.height * 0.30
            self.start_y = self.height * 0.35 
        elif num_players <= 4:
            pct_height = 0.15
            self.card_height = self.height * pct_height
            self.row_height = self.height * 0.18
            self.start_y = self.height * 0.25
        else:
            pct_height = 0.11
            self.card_height = self.height * pct_height
            self.row_height = self.height * 0.13
            self.start_y = self.height * 0.20

        self.card_width = self.card_height * 0.7
        self.slot_spacing = self.card_width * 1.3
        self.card_margin = self.card_width * 0.1
        
        board_total_width = 4 * self.slot_spacing
        self.start_x = (self.width / 2) - (board_total_width / 2)

    def get_player_screen_order(self):
        players = self.game_state.get("players", [])
        if not self.my_pid or self.my_pid not in players:
            return players
        idx = players.index(self.my_pid)
        return players[idx:] + players[:idx]

    def get_slot_position(self, player_index, slot_name):
        colors = ["red", "green", "blue", "yellow", "wild"]
        if slot_name not in colors: return 0, 0
        col_index = colors.index(slot_name)
        
        x = self.start_x + (col_index * self.slot_spacing)
        y = self.start_y + (player_index * self.row_height)
        return x, y

    def get_dynamic_board_click(self, mouse_x, mouse_y):
        sorted_players = self.get_player_screen_order()
        for i, pid in enumerate(sorted_players):
            for color_name in ["red", "green", "blue", "yellow", "wild"]:
                cx, cy = self.get_slot_position(i, color_name)
                if (mouse_x > cx - self.card_width/2 and mouse_x < cx + self.card_width/2 and
                    mouse_y > cy - self.card_height/2 and mouse_y < cy + self.card_height/2):
                    return {"pid": pid, "color": color_name}
        return None

    def _process_server_message(self, delta_time, message):
            print(f"Servidor -> {message}")
            try:
                data = json.loads(message)
                action = data.get("action")
                
                if action == "welcome":
                    self.my_pid = data.get("my_pid")
                    self.status_text = f"Conectado! Soy {self.my_pid}"

                elif action == "play_error":
                    self.status_text = f"Error: {data.get('reason')}"
                    self.selected_card_index = None
                    self.selected_card_data = None
                    self.transplant_source_color = None

                elif action == "play_ok":
                    self.status_text = "Jugada aceptada."

                elif action == "update_state":
                    self.game_state = data.get("state")
                    self.recalculate_layout() 
                    self.status_text = "Estado actualizado."
                    self.winner_pid = self.game_state.get("winner")
                    game_stage = self.game_state.get("game_stage")
                    
                    if game_stage == "game_over":
                         self.status_text = "PARTIDA FINALIZADA"
                    else:
                        self.is_my_turn = (self.game_state.get("current_player") == self.my_pid)
                        self.update_hand_sprites() 
                        
                        if game_stage == "contagion_phase" and self.is_my_turn:
                            self.is_contagion_active = True
                            self.status_text = "¡MODO CONTAGIO! Selecciona TU virus y luego un órgano SANO rival."
                        elif game_stage != "contagion_phase":
                            self.is_contagion_active = False
                            self.contagion_source_color = None
                        
                        if game_stage and game_stage != "waiting_for_players":
                            self.start_button_list.clear()

                elif action == "contagion_continue":
                    self.status_text = "¡Virus propagado! Aún tienes movimientos posibles."
                    self.contagion_source_color = None
            
                elif action == "contagion_finished":
                    self.is_contagion_active = False
                    self.status_text = "Contagio finalizado. Fin del turno."

            except Exception as e:
                print("Error al interpretar JSON:", e)       

    def update_hand_sprites(self):
        self.player_hand_sprites.clear()
        my_hand = self.get_my_hand()
        
        hand_len = len(my_hand)
        start_x = self.width // 2 - ((hand_len * (self.card_width + self.card_margin)) // 2) + (self.card_width // 2)

        for i, card_data in enumerate(my_hand):
            x = start_x + i * (self.card_width + self.card_margin)
            y = self.height * 0.1 
            texture = self.get_card_texture(card_data)
            if texture:
                card_sprite = arcade.Sprite()
                card_sprite.texture = texture
                card_sprite.width = self.card_width
                card_sprite.height = self.card_height
            else:
                card_sprite = arcade.SpriteSolidColor(self.card_width, self.card_height, arcade.color.MAGENTA)
            card_sprite.center_x = x
            card_sprite.center_y = y
            card_sprite.data = card_data
            card_sprite.index = i
            self.player_hand_sprites.append(card_sprite)

    def draw_game_over_screen(self):
        arcade.draw_lrbt_rectangle_filled(0, self.width, 0, self.height, arcade.color.BLACK)
        
        if self.winner_pid == self.my_pid:
            main_text = "¡VICTORIA!"
            sub_text = "Has completado tu cuerpo primero."
            color = arcade.color.GOLD
            icon_text = "🏆"
        else:
            main_text = "DERROTA"
            winner_display = self.winner_pid[:10] if self.winner_pid else "Desconocido"
            nicknames = self.game_state.get("nicknames", {})
            winner_name = nicknames.get(self.winner_pid, winner_display)
            sub_text = f"El ganador ha sido: {winner_name}"
            color = arcade.color.RED_DEVIL
            icon_text = "💀"
            
            
        arcade.draw_text(main_text, self.width/2, self.height/2 + 50, 
                         color, self.font_huge, anchor_x="center", bold=True)
        arcade.draw_text(icon_text, self.width/2, self.height/2 + 150, 
                         color, self.font_icon, anchor_x="center")
        arcade.draw_text(sub_text, self.width/2, self.height/2 - 20, 
                         arcade.color.WHITE, self.font_large, anchor_x="center")
        arcade.draw_text("Cierra la ventana para salir", self.width/2, self.height/2 - 100, 
                         arcade.color.GRAY, self.font_medium, anchor_x="center")

    def on_draw(self):
        self.clear()
        
        stage = self.game_state.get("game_stage")
        if stage == "game_over":
            self.draw_game_over_screen()
            return 

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


        turno_text = "¡TU TURNO!" if self.is_my_turn else "Esperando..."
        arcade.draw_text(f"Estado: {turno_text}", 20, self.height - 40, arcade.color.WHITE, self.font_large)
        arcade.draw_text(self.status_text, 20, self.height - 70, arcade.color.CYAN, self.font_medium)
        
        if self.selected_card_data:
            card_name = self.selected_card_data['name']
            if self.transplant_source_color:
                arcade.draw_text(f"Sel: {card_name} (Origen: {self.transplant_source_color})", 
                                 20, 20, arcade.color.YELLOW, self.font_medium)
            else:
                arcade.draw_text(f"Sel: {card_name}", 20, 20, arcade.color.YELLOW, self.font_medium)

        self.start_button_list.draw()
        arcade.draw_text("INICIAR", self.start_button.center_x, self.start_button.center_y, 
                         arcade.color.BLACK, self.font_normal, anchor_x="center", anchor_y="center")

        board_sprites = arcade.SpriteList()
        sorted_players = self.get_player_screen_order()
        nicknames = self.game_state.get("nicknames", {})
        boards = self.game_state.get("boards", {})
        curr_player_pid = self.game_state.get("current_player")

        for i, pid in enumerate(sorted_players):
            board_data = boards.get(pid, {})
            nickname = nicknames.get(pid, pid[:5])
            _, base_y = self.get_slot_position(i, "red")
            
            color_text = arcade.color.GREEN if pid == self.my_pid else arcade.color.WHITE
            if pid == curr_player_pid:
                color_text = arcade.color.YELLOW
                nickname += " (TURNO)"
            
            
            arcade.draw_text(nickname, 20, base_y, color_text, self.font_normal, anchor_y="center")

            for color_name in ["red", "green", "blue", "yellow", "wild"]:
                x, y = self.get_slot_position(i, color_name)
                
                slot_color = COLOR_MAP[color_name]
                arcade.draw_lrbt_rectangle_outline(
                    left=x - self.card_width/2, right=x + self.card_width / 2,
                    bottom=y - self.card_height / 2, top=y + self.card_height / 2,
                    color=slot_color, border_width=2
                )
                
                slot_data = board_data.get(color_name, {"state": 0, "cards": []})
                cards_in_slot = slot_data.get("cards", [])
                cards_to_draw = list(reversed(cards_in_slot))
                
                STACK_OFFSET = self.card_height * 0.2 
                stack_height = (len(cards_to_draw) - 1) * STACK_OFFSET if cards_to_draw else 0
                card_base_y = y - stack_height / 2
                
                for idx_c, card in enumerate(cards_to_draw):
                    cy = card_base_y + (idx_c * STACK_OFFSET)
                    texture = self.get_card_texture(card) 
                    if texture:
                        temp_sprite = arcade.Sprite()
                        temp_sprite.texture = texture
                        temp_sprite.center_x = x
                        temp_sprite.center_y = cy
                        temp_sprite.width = self.card_width
                        temp_sprite.height = self.card_height
                        board_sprites.append(temp_sprite)

        dp_x, dp_y = self.discard_pile_pos
        arcade.draw_text("Descarte", dp_x, dp_y + self.card_height * 0.6, 
                         arcade.color.WHITE, self.font_normal, anchor_x="center")
        
        top_card = self.game_state.get("top_discard_card")
        if top_card and top_card != 'null':
            texture = self.get_card_texture(top_card)
            if texture:
                temp_sprite = arcade.Sprite()
                temp_sprite.texture = texture
                temp_sprite.center_x = dp_x
                temp_sprite.center_y = dp_y
                temp_sprite.width = self.card_width
                temp_sprite.height = self.card_height
                board_sprites.append(temp_sprite)
        else:
            pile_color = arcade.color.YELLOW if self.is_discarding else arcade.color.GRAY
            arcade.draw_lrbt_rectangle_outline(
                left=dp_x - self.card_width / 2, right=dp_x + self.card_width / 2,
                bottom=dp_y - self.card_height / 2, top=dp_y + self.card_height / 2,
                color=pile_color, border_width=3
            )
        
        if self.is_discarding and len(self.selected_card_indices) > 0:
            arcade.draw_text(f"{len(self.selected_card_indices)} / 3", dp_x, dp_y - self.card_height, 
                             arcade.color.YELLOW, self.font_normal, anchor_x="center")

        board_sprites.draw()

    def on_mouse_press(self, x, y, button, modifiers):
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

        if self.is_contagion_active:
            clicked_slot = self.get_dynamic_board_click(x, y)
            if clicked_slot:
                if clicked_slot['pid'] == self.my_pid:
                    self.contagion_source_color = clicked_slot['color']
                    my_board = self.game_state['boards'][self.my_pid]
                    source_data = my_board.get(self.contagion_source_color, {})
                    cards = source_data.get('cards', [])
                    is_wild = False
                    for c in cards:
                        if c.get('type') == 'virus' and c.get('color') == 'wild':
                            is_wild = True
                            break
                    self.source_is_wild = is_wild
                    msg_extra = " (ES COMODÍN)" if is_wild else ""
                    self.status_text = f"Origen: {self.contagion_source_color.upper()}{msg_extra}. Elige víctima."
                    return

                elif clicked_slot['pid'] != self.my_pid:
                    if not self.contagion_source_color:
                        self.status_text = "Primero selecciona TU órgano infectado."
                        return
                    target_color = clicked_slot['color']
                    target_pid = clicked_slot['pid']
                    
                    msg = {
                        "action": "contagion_step",
                        "target_pid": target_pid,
                        "color": target_color,
                        "source_color": self.contagion_source_color
                    }
                    print(f"Enviando paso contagio: {msg}")
                    self.network.send(json.dumps(msg))
                    self.contagion_source_color = None
                    self.source_is_wild = False
                    return
            return

        dp_x, dp_y = self.discard_pile_pos
        if (x > dp_x - self.card_width/2 and x < dp_x + self.card_width/2 and
            y > dp_y - self.card_height/2 and y < dp_y + self.card_height/2):
            if self.is_discarding:
                self.send_discard_cards()
            else:
                self.is_discarding = True
                self.selected_card_index = None
                self.selected_card_data = None
                self.selected_card_indices = []
                self.status_text = "DESCARTE: Elige hasta 3 cartas. Clic aquí para confirmar."
            return

        cards_clicked = arcade.get_sprites_at_point((x, y), self.player_hand_sprites)
        if cards_clicked:
            clicked_card_sprite = cards_clicked[-1]
            idx = clicked_card_sprite.index
            if self.is_discarding:
                if idx in self.selected_card_indices:
                    self.selected_card_indices.remove(idx)
                else:
                    if len(self.selected_card_indices) < 3:
                        self.selected_card_indices.append(idx)
                    else:
                        self.status_text = "Límite de 3 cartas."
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
                    c_name = self.selected_card_data['name']
                    if c_name == 'transplant':
                        self.status_text = f"Trasplante: Elige TU órgano, luego el SUYO."
                    else:
                        self.status_text = f"Seleccionado: {c_name}. Elige objetivo."
                return

        if self.selected_card_data and not self.is_discarding:
            clicked_slot = self.get_dynamic_board_click(x, y)
            if clicked_slot:
                if self.selected_card_data['name'] == 'transplant':
                    self.handle_transplant_click_dynamic(clicked_slot)
                else:
                    self.handle_normal_click_dynamic(clicked_slot)

    def handle_transplant_click_dynamic(self, clicked_slot):
        if self.transplant_source_color is None:
            if clicked_slot['pid'] == self.my_pid:
                self.transplant_source_color = clicked_slot['color']
                self.status_text = f"Origen '{self.transplant_source_color}'. Ahora elige RIVAL."
            else:
                self.status_text = "Trasplante: Primero TU órgano."
        else:
            if clicked_slot['pid'] != self.my_pid:
                self.send_play_card(
                    clicked_slot['pid'], 
                    clicked_slot['color'], 
                    player_color=self.transplant_source_color
                )
            else:
                self.status_text = "Trasplante: El segundo clic debe ser un RIVAL."

    def handle_normal_click_dynamic(self, clicked_slot):
        self.send_play_card(clicked_slot['pid'], clicked_slot['color'])

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

    def send_discard_cards(self):
        if not self.selected_card_indices:
            self.status_text = "Selecciona cartas para descartar."
            return
        cards_to_discard = []
        my_hand = self.get_my_hand() 
        for i in self.selected_card_indices:
            try:
                cards_to_discard.append(my_hand[i]) 
            except IndexError:
                pass
        msg = {
            "action": "discard_cards",
            "cards": cards_to_discard
        }
        print(f"Enviando descarte: {len(cards_to_discard)} cartas")
        self.network.send(json.dumps(msg))
        self.selected_card_indices = []
        self.is_discarding = False

def main():
    print("=== CLIENTE VIRUS 6 JUGADORES ===")
    server_ip = input("IP del servidor (Enter para 127.0.0.1): ").strip()
    if not server_ip:
        server_ip = "127.0.0.1"
    
    nickname = input("Tu Nickname: ").strip()
    if not nickname:
        nickname = "Jugador"
    
    game = VirusClient(nickname)
    game.server_ip = server_ip
    game.connect_and_join()
    arcade.run()

if __name__ == "__main__":
    main()