import serial  # type: ignore

import base64
import struct
import time
from typing import Optional


BAUD_RATE = 2000000
NENTRY = 4


class PinProtectedBackup:
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

    def status(self, slot: int) -> bool:
        if slot >= NENTRY:
            return False
        self._ser.write(b"\x01" + struct.pack(">B", slot))
        valid = self._ser.read(1)
        return valid != b"\x00"

    def delete(self, slot: int) -> bool:
        if slot >= NENTRY:
            return False
        self._ser.write(b"\x02" + struct.pack(">B", slot))
        was_valid = self._ser.read(1)
        return was_valid != b"\x00"

    def store(self, slot: int, pin: bytes, data: bytes) -> bool:
        if len(pin) != 4:
            raise ValueError("pin must be 4 bytes")
        if len(data) != 16:
            raise ValueError("data must be 16 bytes")
        if slot >= NENTRY:
            return False
        self._ser.write(b"\x03" + struct.pack(">B", slot) + pin + data)
        ok = self._ser.read(1)
        return ok != b"\x00"

    def retrieve(self, slot: int, pin: bytes) -> Optional[bytes]:
        if len(pin) != 4:
            raise ValueError("pin must be 4 bytes")
        if slot >= NENTRY:
            return None
        self._ser.write(b"\x04" + struct.pack(">B", slot) + pin)
        status = self._ser.read(1)
        if status != b"\x03":
            return None
        return self._ser.read(16)
