import serial  # type: ignore

import base64
import struct
import time


BAUD_RATE = 2000000


class PasswordHasher:
    # serial_port is like '/dev/tty.usbserial-AJ02ZJPV'
    def __init__(self, serial_port: str) -> None:
        self._ser = serial.Serial(
            serial_port,
            baudrate=BAUD_RATE,
            parity=serial.PARITY_NONE,
            stopbits=serial.STOPBITS_ONE,
            bytesize=serial.EIGHTBITS,
            rtscts=True,
        )
        self._ser.flushInput()

    def set_secret(self, secret: bytes) -> None:
        if len(secret) != 20:
            raise ValueError("secret must be exactly 20 bytes")
        self._ser.write(b"\x01" + secret)
        self._ser.read(1)

    def get_hash(self, message: bytes) -> str:
        if len(message) != 32:
            raise ValueError("message must be exactly 32 bytes")
        self._ser.write(b"\x02" + message)
        digest: bytes = self._ser.read(32)
        return digest.hex()
