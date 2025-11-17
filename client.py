import arcade
import json
import socket
import threading
import struct

# (No importamos 'card' ni 'os' ni 'random' porque no los usaremos)

# ============================
# CONFIGURACION
# ============================
SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720
SCREEN_TITLE = "Hero Realms LP - PRUEBA DE CONEXION"

# ============================
# CLIENTE DE RED
# (Esta clase está intacta, es la que queremos probar)
# ============================
class NetworkClient:
    
    def send(self, data):
        if self.connected:
            try:
                # 1. Codificar el mensaje
                msg = data.encode('utf-8')
                # 2. Crear prefijo de 4 bytes (Big-endian)
                length_prefix = struct.pack('>I', len(msg))
                # 3. Enviar prefijo + mensaje
                self.sock.sendall(length_prefix + msg)
                print(f"Enviado (Prefijo {len(msg)}): {data}")
            except Exception as e:
                print("Error al enviar:", e)
                
    def __init__(self, host, port=4000):
        self.host = host
        self.port = port
        self.sock = None
        self.connected = False
        self.last_message = ""
        self.on_message = None  # callback

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
                # [FIX IMPORTANTE]
                # El servidor Erlang usa {packet, 4}, NO {packet, line}.
                # Necesitamos leer el prefijo de 4 bytes primero.
                
                # 1. Leer los 4 bytes del prefijo (longitud)
                prefix_data = self.sock.recv(4)
                if not prefix_data:
                    break
                
                msg_len = struct.unpack('>I', prefix_data)[0]
                
                # 2. Leer exactamente esa cantidad de bytes
                data = self.sock.recv(msg_len)
                if not data:
                    break

                msg = data.decode("utf-8").strip()
                
                if self.on_message:
                    self.on_message(msg)
            except Exception as e:
                print(f"Error en _listen: {e}")
                break
        self.connected = False
        print("Conexión cerrada con el servidor.")

# ============================
# CLIENTE GRAFICO (Modificado)
# ============================
class HeroRealmsClient(arcade.Window):
    def __init__(self, server_ip):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_TITLE, update_rate=1/60)
        # Usamos un fondo de color simple, sin assets
        arcade.set_background_color(arcade.color.DARK_BLUE_GRAY)

        # Red (Esto es lo que queremos probar)
        self.network = NetworkClient(server_ip)
        self.network.on_message = self._process_erlang_message
        self.network.connect()

        # Estado del juego (Esto se actualiza por la red)
        self.current_turn = "Esperando jugadores"
        self.player_role = "Desconocido"

        # Listas de sprites vacías, solo para que 'on_draw' no falle
        self.player_hand = arcade.SpriteList()
        self.market = arcade.SpriteList()
        self.end_turn_button_list = arcade.SpriteList()

        # No llamamos a _create_sample_hand() ni _create_market()
        # porque no tenemos los assets ni 'card.py'

    def _create_sample_hand(self):
        # Dejamos esta función vacía
        pass
    
    def _create_market(self):
        # Dejamos esta función vacía
        pass

    # ============================
    # EVENTOS GRAFICOS
    # ============================

    def on_draw(self):
        self.clear()

        # Ya no dibujamos las cartas ni el fondo
        
        # Mostrar turno y conexion
        # ¡Esta es la prueba visual de que la red funciona!
        arcade.draw_text(f"Conexión: {'Conectado' if self.network.connected else 'Desconectado'}",
                         20, SCREEN_HEIGHT - 40, arcade.color.WHITE, 16)

        arcade.draw_text(f"Mi Rol: {self.player_role}",
                         20, SCREEN_HEIGHT - 70, arcade.color.CYAN, 18)

        color_turno = arcade.color.GREEN if self.current_turn == self.player_role else arcade.color.GRAY
        turno_text = "¡Es tu turno!" if self.current_turn == self.player_role else "Turno del oponente"
        
        arcade.draw_text(f"Estado: {turno_text} (Turno actual: {self.current_turn})",
                         20, SCREEN_HEIGHT - 100, color_turno, 18)
        
        arcade.draw_text("Haz clic en cualquier parte para enviar un PING al servidor",
                         20, 40, arcade.color.LIGHT_GRAY, 14)

    ### MOUSE
    def on_mouse_motion(self, x, y, dx, dy):
        # Vaciamos esta función porque no hay cartas que animar
        pass

    ### MOUSE CLICK
    def on_mouse_press(self, x, y, button, modifiers):
        # Esta es la prueba de ENVÍO
        # Mantenemos la lógica de PING
        if self.network.connected:
            print("Enviando PING...")
            msg = json.dumps({"action": "ping"})
            self.network.send(msg)
        return

    # ============================
    # PROCESAMIENTO DE MENSAJES
    # (Esta función es clave para la prueba de RECEPCIÓN)
    # ============================

    def _process_erlang_message(self, message):
            msg = message.strip()
            print(f"Servidor -> {msg}") # Descomentar para debug
            
            if msg.startswith("{"):
                try:
                    data = json.loads(msg)
                    action = data.get("action")
                    
                    if action == "welcome":
                        role = data.get("role")
                        if role:
                            self.player_role = role
                            print(f"**ROL ASIGNADO:** {self.player_role}")

                    elif action == "update":
                        turn = data.get("turn")
                        self.current_turn = turn
                        print(f"**NUEVO TURNO:** {self.current_turn}")
                        
                except Exception as e:
                    print("Error al interpretar JSON:", e)


# ============================
# MAIN (Sin cambios)
# ============================

def main():
    server_ip = input("IP del servidor Erlang: ").strip()
    game = HeroRealmsClient(server_ip)
    arcade.run()

if __name__ == "__main__":
    main()