package arm64

def pad16(size: Int): Int = (size + 15) & ~15
