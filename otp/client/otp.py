import serial  # type: ignore

import base64
import struct
import time


BAUD_RATE = 2000000


class OTP:
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

    # secret is base32-encoded, e.g. "U24AABH2OGRDTWMPCOG4ADCNGZJ5FHQJ"
    def set_secret(self, secret: str) -> None:
        self._ser.write(b"\x01" + base64.b32decode(secret))
        self._ser.read(1)

    def audit(self) -> int:
        self._ser.write(b"\x03")
        c_raw: bytes = self._ser.read(8)
        c: int = struct.unpack(">Q", c_raw)[0]
        return c

    def otp(self, c: int) -> str:
        c_enc = struct.pack(">Q", c)
        self._ser.write(b"\x02" + c_enc)
        ok = self._ser.read(1)
        if ok != b"\x02":
            raise ValueError("rewinding counter")
        v = struct.unpack(">L", self._ser.read(4))[0]
        return f"{v:06d}"

    # not part of the spec, but included for convenience
    #
    # time is computed on the host
    def totp(self) -> str:
        t = int(time.time())
        c = t // 30
        return self.otp(c)
