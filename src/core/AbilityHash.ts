export class AbilityHash {
  public static sha1(message: string): string {
    const msgBytes = AbilityHash.stringToBytes(message);
    const msgBitLength = msgBytes.length * 8;

    const withOne = new Uint8Array(msgBytes.length + 1);
    withOne.set(msgBytes, 0);
    withOne[msgBytes.length] = 0x80;

    let zeroBytes = (56 - (withOne.length % 64) + 64) % 64;
    const padded = new Uint8Array(withOne.length + zeroBytes + 8);
    padded.set(withOne, 0);

    const bitLenHigh = Math.floor(msgBitLength / 0x100000000);
    const bitLenLow = msgBitLength >>> 0;

    padded[padded.length - 8] = (bitLenHigh >>> 24) & 0xff;
    padded[padded.length - 7] = (bitLenHigh >>> 16) & 0xff;
    padded[padded.length - 6] = (bitLenHigh >>> 8) & 0xff;
    padded[padded.length - 5] = bitLenHigh & 0xff;
    padded[padded.length - 4] = (bitLenLow >>> 24) & 0xff;
    padded[padded.length - 3] = (bitLenLow >>> 16) & 0xff;
    padded[padded.length - 2] = (bitLenLow >>> 8) & 0xff;
    padded[padded.length - 1] = bitLenLow & 0xff;

    let h0 = 0x67452301;
    let h1 = 0xefcdab89;
    let h2 = 0x98badcfe;
    let h3 = 0x10325476;
    let h4 = 0xc3d2e1f0;

    const w = new Array<number>(80);

    for (let i = 0; i < padded.length; i += 64) {
      for (let j = 0; j < 16; j++) {
        const idx = i + j * 4;
        w[j] =
          (padded[idx] << 24) | (padded[idx + 1] << 16) | (padded[idx + 2] << 8) | padded[idx + 3];
      }

      for (let j = 16; j < 80; j++) {
        w[j] = AbilityHash.leftRotate(w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16], 1);
      }

      let a = h0;
      let b = h1;
      let c = h2;
      let d = h3;
      let e = h4;

      for (let j = 0; j < 80; j++) {
        let f: number;
        let k: number;

        if (j < 20) {
          f = (b & c) | (~b & d);
          k = 0x5a827999;
        } else {
          if (j < 40) {
            f = b ^ c ^ d;
            k = 0x6ed9eba1;
          } else {
            if (j < 60) {
              f = (b & c) | (b & d) | (c & d);
              k = 0x8f1bbcdc;
            } else {
              f = b ^ c ^ d;
              k = 0xca62c1d6;
            }
          }
        }

        const temp = (AbilityHash.leftRotate(a, 5) + f + e + k + (w[j] | 0)) | 0;

        e = d;
        d = c;
        c = AbilityHash.leftRotate(b, 30);
        b = a;
        a = temp;
      }

      h0 = (h0 + a) | 0;
      h1 = (h1 + b) | 0;
      h2 = (h2 + c) | 0;
      h3 = (h3 + d) | 0;
      h4 = (h4 + e) | 0;
    }

    return [
      AbilityHash.toHex32(h0),
      AbilityHash.toHex32(h1),
      AbilityHash.toHex32(h2),
      AbilityHash.toHex32(h3),
      AbilityHash.toHex32(h4),
    ].join('');
  }

  private static leftRotate(value: number, bits: number): number {
    return ((value << bits) | (value >>> (32 - bits))) >>> 0;
  }

  private static toHex32(num: number): string {
    return (num >>> 0).toString(16).padStart(8, '0');
  }

  private static stringToBytes(str: string): Uint8Array {
    if (typeof TextEncoder !== 'undefined') {
      const encoder = new TextEncoder();
      return encoder.encode(str);
    } else {
      const buf = Buffer.from(str, 'utf8');
      return new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
    }
  }
}
