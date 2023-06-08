CARRY = 0
ZERO = 1
INTERUPT_DISABLE = 2
DECIMAL = 3
BREAK = 4
OVERFLOW = 6
NEGATIVE = 7

regA = 0x00
regX = 0x00
regY = 0x00
regFlag = [0,0,0,0,0,0,0,0]
regStack = 0x00
#regAddress = 0x0000
regPC = 0x0000

#import decode
#

memory = []
iCache = [] #256 x 32
dCache = [] #1024 x 32
prefetchBuffer = []   #size?
writebackBuffer = [] #size?

#These could be implemented as instances of a pipeReg class
fetchRegister = {}
decodeRegister = { 'instruction' : [], 'branch': -1 } # -1 = no branch, 0 = predict not taken, 1 = predict taken
addressRegister = { 'instruction' : [], 'branch': -1, 'decoded': {}}
indexRegister = { 'instruction' : [], 'branch': -1, 'decoded': {}}
dataRegister = { 'instruction' : [], 'branch': -1, 'decoded': {}}
executeRegister = { 'instruction' : [], 'branch': -1, 'decoded': {}}
writebackRegister = { 'instruction' : [], 'branch': -1, 'decoded': {}}

pipeRegisters = [fetchRegister, decodeRegister, addressRegister, indexRegister, dataRegister, executeRegister, writebackRegister]

memBusy = False

class cacheLine:
    def __init__(self, tag, values):
        self.tag = tag
        self.values = values

def initializeMemory(value):
    for x in range(65536):
        memory.append(0x00)

def dumpMemory(start, stop):
    for x in range(int((stop-start)/0x10)):                 #iterate over lines of 16 bytes
        base = start + x * 0x10                             #base address for each line
        print('0x%04x: ' %base, end = '')                   
        for i in range(0x10):                               #iterate over addresses within line
            print('0x%02x ' %(memory[base + i]), end = '')
        print()

def readMem(address, type):
    match type:
        case 'instruction':
            cAddress = calcCacheAddress(iCache, address)
            if(iCache[cAddress].tag == address):
                return iCache[cAddress]
        case 'data':
            cAddress = calcCacheAddress(dCache, address)
            if(dCache[cAddress].tag == address):
                return dCache[cAddress]
    #need to check write through buffer
    buffCheck = checkWriteBackBuffer(address)
    if(buffCheck != -1):
        return(writeBackBuffer[buffCheck])
    memBusy = True
    return(memory[address])

#evict line and write through?
def writeMem(address, data, type):
    match type:
        case 'instruction':
            cAddress = calcCacheAddress(iCache, address)
            if(iCache[cAddress].tag == address):
                iCache[cAddress] = data
        case 'data':
            cAddress = calcCacheAddress(dCache, address)
            if(dCache[cAddress].tag == address):
                dCache[cAddress] = data
    writeBack(address, data)
    return

def checkDataHazard(data, stage):
    for i in range(stage+1, 6):
        if(data == pipeRegisters[i]['decoded']['target']):
            return i
    return -1

def effectiveAddress(mode):
    print()

def isBranch(instruction):
    return((instruction[0] & 0b0001_1111) == 0b0001_0000)


def pipeFetch():
    #@data
        #3 bytes from instruction mem
        #PC
    #@ops
        #load from prefetch buffer => instruction cache => main mem
        #branch prediction
        #update PC 
        #refill prefetch buffer
    instruction = []
    instruction.append(readMem(pc, 'instruction')) 
    instruction.append(readMem(pc+1, 'instruction')) 
    instruction.append(readMem(pc+2, 'instruction')) 
    if(isBranch(instruction)):
        #1 = branch taken, 0 = branch not taken, -1 = no branch
        prediction = branchPredict()
        if(prediction):
            pc = branchTarget()
            decodeRegister['branch'] = 1
        else:
            decodeRegister['branch'] = 0    
    else:
        pc += calcInstructionLength(instruction[1])
        decodeRegister['branch'] = -1

    decodeRegister['instruction'] = instruction
    
def pipeDecode():
    #@data
        #instruction + immediate values
        #x register for pre-index
    #@ops
        #access register values
            #less data stalls if register values are accessed as late as possible
        #prepare ALU+Index control signals
    #prepares ALU + Index ops, maybe returns functions
    instruction = decodeRegister['instruction']
    decoded = decode(instruction)

    addressRegister['branch'] = decodeRegister['branch']
    addressRegister['instruction'] = decodeRegister['instruction']
    addressRegister['decoded'] = decoded
    return

def pipeAddress():
    #@data 
        #base address(register | immediate)
        #index value(register | immediate)
        #address memory(read address)
    #@ops
        #pre-index address
        #read indirect address from address mem
        #handle indirect jumps
    mode = addressRegister['decoded']['addressingMode']
    instruction = addressRegister['instruction']
    address = 0


    if(mode == 'indirect' or mode == 'indirectY'):
        address = addressRegister['decoded']['immediate']
        address = readMem(address, 'data') 
    elif(mode == 'indirectX'):
        if(checkDataHazard('regX', 2) != -1):
            indexRegister['instruction'][0] = 0x00 #maybe not necessary?
            indexRegister['decoded'] =  insertNOP()
            return
        #immediate = 8bit zero page address
        address = regX + addressRegister['decoded']['immediate']
        address = readMem(address, 'data')

    indexRegister['branch'] = addressRegister['branch']
    indexRegister['instruction'] = addressRegister['instruction']
    indexRegister['decoded'] = addressRegister['decoded']
    indexRegister['decoded']['address'] = address
    return

#This could potentially be merged into the previous stage
#This would depend heavily on the excicution time of other stages, a tough decision to make in software
def pipeIndex():
    #@data
        #index value
        #fetched indirect address
    #@ops
        #apply post-index
    if(indexRegister['decoded']['addressingMode'] == 'indirectY'): 
        if(checkDataHazard('regY', 3) != -1):
            dataRegister['instruction'][0] = 0x00 #maybe not necessary?
            dataRegister['decoded'] =  insertNOP()
            return
        indexRegister['decoded']['address'] += regY
    dataRegister['branch'] = indexRegister['branch']
    dataRegister['instruction'] = indexRegister['instruction']
    dataRegister['decoded'] = indexRegister['decoded']

def pipeData():
    #@data
        #fully resolved data address
    #@ops
        #fetch data from indexed/indirect address
    
    #may need logic to avoid mem access conflict
    operands = dataRegister['decoded']['operands']
    if('memory' in operands):
        dataRegister['decoded']['operands'][operands.index('memory')] = readMem(dataRegister['decoded']['address'], 'data')

    executeRegister['branch'] = dataRegister['branch']
    executeRegister['instruction'] = dataRegister['instruction']
    executeRegister['decoded'] = dataRegister['decoded']
    

def pipeExecute():
    print()
    #@data
        #immediate value | data from mem
        #register value
        #control signals
    #@ops
        #ALU ops on registers and fetched data
        #resolve branches

    #check for data hazards
    for i in executeRegister['decoded']['operands']:
        if(checkDataHazard(i, 4) != -1):
            indexRegister['instruction'][0] = 0x00 #maybe not necessary?
            indexRegister['decoded'] =  insertNOP()
            return
    operand1 = fetchOperand(executeRegister['decoded']['operands'][0])
    operand2 = fetchOperand(executeRegister['decoded']['operands'][1])
    executeRegister['decoded']['result'] = executeRegister['decoded']['operation'](operand1, operand2)


    writebackRegister['branch'] = executeRegister['branch']
    writebackRegister['instruction'] = executeRegister['instruction']
    writebackRegister['decoded'] = executeRegister['decoded']

def pipeWriteBack():
    print()
    #@data 
        #post ALU data value
        #address to be written to(register | mem)
    #@ops
        #write back to registers
        #update flags
    target = writebackRegister['decoded']['target']

def cycle():
    print()
    #memBusy = False



#initializeMemory(0x00)

#dumpMemory(0x0000, 0x00ff)














#


def decode(instruction):
    #decoded dict keys
        #OP = function to excecute
        #addressingMode = 
    instruction = instruction[0]
    operation = NOP
    operand = ''
    addressing = ''
    
    decoded = {'operation': NOP, 'operands':[]}
    memAccess = False
    aaa = instruction >> 5
    bbb = (instruction & 0b0001_1100) >> 2
    cc = instruction & 0b0000_0011
    #Group One
    if(cc == 0b01):
        #(zero page,X)
        if(bbb == 0b000):
            addressing = 'indirectX'
        #zero page
        elif(bbb == 0b001):
            addressing = 'zeroPage'
        ##immediate
        elif(bbb == 0b010):
            addressing = 'immediate'
        #absolute
        elif(bbb == 0b011):
            addressing = 'absolute'
        #(zero page),Y
        elif(bbb == 0b100):
            addressing = 'indirectY'
        #zero page,X
        elif(bbb == 0b101):
            addressing = 'zeroPageX'
        #absolute,Y
        elif(bbb == 0b110):
            addressing = 'absoluteY'
        #absolute,X
        elif(bbb == 0b111):
            addressing = 'absoluteX'
        
        #ORA
        if(aaa == 0b000):
            operation = OR
            operand = 'regA'
        #AND
        elif(aaa == 0b001):
            operation = AND 
            operand = 'regA'
        #EOR
        elif(aaa == 0b010):
            operation = EOR
            operand = 'regA'
        #ADC
        elif(aaa == 0b011):
            operation = ADC
            operand = 'regA'
        #STA
        elif(aaa == 0b100):
            operation = STORE
            operand = "regA"
        #LDA
        elif(aaa == 0b101):
            operation = LOAD
            operand = "regA"
        #CMP
        elif(aaa == 0b110):
            operation = CMP
            operand = "regA"
        #SBC
        elif(aaa == 0b111):
            operation = SBC
            operand = "regA"


    #Group Two
    elif(cc == 0b10):
        ##immediate
        if(bbb == 0b000):
            addressing = 'immediate'
        #zero page
        elif(bbb == 0b001):
            addressing = 'zeroPage'
        #accumulator
        elif(bbb == 0b010):
            addressing = 'accumulator'
        #absolute
        elif(bbb == 0b011):
            addressing = 'absolute'
        #zero page, X
        elif(bbb == 0b101):
            addressing = 'zeroPageX'
        #absolute, X
        elif(bbb == 0b111):
            addressing = 'absoluteX'
        
        #operand can be regA or MEM, addressed through 'accumulator' addressing mode
        #ASL
        if(aaa == 0b000):
            operation = SHIFTL
            if(addressing == 'accumulator'):
                operand = 'regA'
            else:
                operand = 'memory'
        #ROL
        elif(aaa == 0b001):
            operation = ROTATEL
            if(addressing == 'accumulator'):
                operand = 'regA'
            else:
                operand = 'memory'
        #LSR
        elif(aaa == 0b010):
            operation = SHIFTR
            if(addressing == 'accumulator'):
                operand = 'regA'
            else:
                operand = 'memory'
        #ROR
        elif(aaa == 0b011):
            operation = ROTATER
            if(addressing == 'accumulator'):
                operand = 'regA'
            else:
                operand = 'memory'
        #STX
        elif(aaa == 0b100):
            operation = STORE
            operand = 'regX'
        #LDX
        elif(aaa == 0b101):
            operation = LOAD
            operand = 'regX'
        #DEC
        elif(aaa == 0b110):
            operation = DEC
            operand = 'memory'
        #INC
        elif(aaa == 0b111):
            operation = INC
            operand = 'memory'

    #Group Three
    elif(cc == 0b00):
        ##immediate
        if(bbb == 0b000):
            addressing = 'immediate'
        #zero page
        elif(bbb == 0b001):
            addressing = 'zeroPage'
        #absolute
        elif(bbb == 0b011):
            addressing = 'absolute'
        #zero page,X
        elif(bbb == 0b101):
            addressing = 'zeroPageX'
        #absolute,X
        elif(bbb == 0b111):
            addressing = 'absoluteX'
        
        #BIT
        if(aaa == 0b001):
            operation = BIT
            operand = 'memory'
        #JMP
        elif(aaa == 0b010):
            operation = JMP
            operand = 'absolute'
        #JMP(abs)
        elif(aaa == 0b011):
            operation = JMPI
            operand = 'absolute'
        #STY
        elif(aaa == 0b100):
            operation = STORE
            operand = 'regY'
        #LDY
        elif(aaa == 0b101):
            operation = LOAD
            operand = 'regY'
        #CPY
        elif(aaa == 0b110):
            operation = CMP
            operand = 'regY'
        #CPX
        elif(aaa == 0b111):
            operation = CMP
            operand = 'regX'

    #conditional branches
    elif((instruction & 0b0001_1111) == 0b0001_0000):
        xx = instruction >> 6
        y = (instruction & 0b0010_0000) >> 5

        addressing = 'relative'
        operation = BRANCH

        #operand = FlagReg index
        #This is sketchy 
        if(xx == 0b00):
            operand = NEGATIVE
        elif(xx == 0b01):
            operand = OVERFLOW
        elif(xx == 0b10):
            operand = CARRY
        elif(xx == 0b11):
            operand = ZERO

    #interrupt and subroutine
    if(instruction):
        

    if(operand == ''):
        raise Exception('Illegal Instruction')
    
    decoded['operation'] = operation
    decoded['operands'][0] = operand
    decoded['addressing'] = addressing
    return decoded


def twosCompliment(in1):
    out = in1
    if(in1 >> 7):
        out = in1 - 0b1_0000_0000
    return out

#in1 = flag value
#in2 = bit to compare
#true = taken
def BRANCH(in1, in2):
    return in1 == in2

#return 16 bit combination of high byte(in1) and low byte(in2)
#should be caught by branch predictor, so PC doesn't need to be set
def JMP(in1, in2):
    return (in1 << 8) + in2

#implementation is the same as JMP, seperate OP
def JMPI(in1, in2):
    return (in1 << 8) + in2

#in1 = regA, in2 = mem
def BIT(in1, in2):
    regFlag[ZERO] = not (in1-in2)
    regFlag[NEGATIVE] = in2 >> 7
    regFlag[OVERFLOW] = (in2 & 0b0100_0000) >> 6
    
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

#Add w Carry
def ADC(in1, in2):
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

#Subtract w Carry
def SBC(in1, in2):
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

#REG - MEM
def CMP(in1, in2):
    if(in1 == in2):
        regFlag[ZERO] = 1
        regFlag[CARRY] = 1
        regFlag[NEGATIVE] = 0
    elif(in1 < in2):
        regFlag[ZERO] = 0
        regFlag[CARRY] = 0
        regFlag[NEGATIVE] = (in1 - in2) >> 7
    else:
        regFlag[ZERO] = 0
        regFlag[CARRY] = 1
        regFlag[NEGATIVE] = (in1 - in2) >> 7

def LOAD(in1):
    regFlag[NEGATIVE] = in1 >> 7 
    regFlag[ZERO] = in1 == 0
    return in1

def STORE(in1):
    return

def NOP():
    return
