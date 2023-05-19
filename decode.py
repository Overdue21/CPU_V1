#import cpu
CARRY = 0
ZERO = 1
INTERUPT_DISABLE = 2
DECIMAL = 3
OVERFLOW = 6
NEGATIVE = 7
regFlag = [0,0,0,0,0,0,0,0]

microcode = [{},{}]

def twosCompliment(in1):
    out = in1
    if(in1 >> 7):
        out = in1 - 0b1_0000_0000
    return out

def decode(instruction):
    decode = microcode[instruction[0]]

def OR(in1, in2):
    result = in1 | in2
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def AND(in1, in2): 
    result = in1 & in2
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def EOR(in1, in2):
    result = in1 ^ in2
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def SHIFTL(in1):
    result = (in1 << 1) & (0b0_1111_1111)
    regFlag[CARRY] = ((in1 << 1 & 0b1_0000_0000)  != 0)
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def SHIFTR(in1):
    result = in1 >> 1
    regFlag[CARRY] = in1 & 0b0000_0001
    regFlag[NEGATIVE] = 0
    regFlag[ZERO] = (result == 0)
    return result

def ROTATEL(in1):
    carry = in1 >> 7
    result = ((in1 << 1) | carry) & (0b0_1111_1111)
    regFlag[CARRY] = carry
    regFlag[NEGATIVE] = result >> 7 
    regFlag[ZERO] = (result == 0)
    return result

def ROTATER(in1):
    carry = in1 & 0b0000_0001
    result = (in1 >> 1) + (carry << 7)
    regFlag[CARRY] = carry
    regFlag[NEGATIVE] = result >> 7 
    regFlag[ZERO] = (result == 0)
    return result

def ADD(in1, in2):
    result = in1 + in2 + regFlag[CARRY]
    carry = result >> 8 
    t1 = twosCompliment(in1)
    t2 = twosCompliment(in2)
    if(t1 + t2 > 127 or t1 + t2 < -128):
        regFlag[OVERFLOW] = 1
    else:
        regFlag[OVERFLOW] = 0
    regFlag[CARRY] = carry
    regFlag[NEGATIVE] = result >> 7 
    regFlag[ZERO] = (result == 0)
    return result - (carry << 8)

def SUB(in1, in2):
    result = in1 - in2 - regFlag[CARRY]
    carry = (in1 >= in2)
    t1 = twosCompliment(in1)
    t2 = twosCompliment(in2)
    if(t1 - t2 > 127 or t1 - t2 < -128):
        regFlag[OVERFLOW] = 1
    else:
        regFlag[OVERFLOW] = 0
    regFlag[CARRY] = carry
    regFlag[NEGATIVE] = result >> 7 
    regFlag[ZERO] = (result == 0)
    print(result & 0xff)
    return result & 0xff

def DEC(in1):
    result = in1 - 1
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def INC(in1):
    result = in1 + 1
    regFlag[NEGATIVE] = result >> 7
    regFlag[ZERO] = (result == 0)
    return result

def LOAD(in1):
    regFlag[NEGATIVE] = in1 >> 7 
    regFlag[ZERO] = in1 == 0
    return in1

ß

