' RAM del Z80
Dim Shared RAM(65535) As Integer 

' guarda el amximo de ROMS leidas, para la rutina POKE, para que no grabe en ROM
Dim Shared MAXROM As Integer

' para depuracion solo: es la zona de RAM que deseamos ver en pantalla
Dim Shared verram As Integer
Dim Shared dibujar As Integer ' para refrescar la pantalla cada 320 lineas (INS) ejecutadas

' para que salga info por pantalla
Dim Shared deb As Byte = 0 ' 0 NO DEB, 1 DEB REGISTROS, 2 DEB REGISTROS+POKES-PEEKS
Dim Shared VelocidadReal As Byte = 1 ' 0 sin control de velocidad, 1 velocidad Z80 real
'Dim Shared x2 As Integer = 1 ' doble tamaño de pantalla

' para saber cuando llamar a INTERRUPT()
Dim Shared TotalStates As Integer ' estados a ejecutar (velocidad de la CPU) 3.5mhz=3.5*1000000/50=70000 aprox. 69888
Dim Shared WastedStates As Integer ' estados ejecutados empezando en 'TotalStates' bajando hasta 0

' uso interno de INTERRUPT()
Dim Shared InterruptDelay   As Integer ' maximo error permitido antes de frenar al "pc" en milisegundos (normal=20)
Dim Shared DelayOverage     As Integer ' control interno de milisegundos sobrantes para las IRQ
Dim Shared TiempoReal   As Integer ' reloj del pc (TIMER()=milisegundos) para saber los tiempos reales trascurridos
Dim Shared interruptCounter As Integer ' contador total de IRQ ejecutadas

' Interrupt registers and flip-flops, and refresh registers
Dim Shared intI As Integer
Dim Shared intR As Integer
Dim Shared intRTemp As Integer
Dim Shared intIFF1 As Integer
Dim Shared intIFF2 As Integer
Dim Shared intIM As Integer

' usada para acelerar calculos (matriz precalculada)
Dim Shared Parity(256) As Integer

' tabla de divisiones, mas rapido que dividir en tiempo real 
Dim Shared Div4    (82000) As Integer
Dim Shared Div32   (82000) As Integer
Dim Shared Div256  (82000) As Integer 
Dim Shared Div16384(82000) As Integer 

' Main Z80 registers
Dim Shared regA As Integer
Dim Shared regHL As Integer
Dim Shared regB As Integer
Dim Shared regC As Integer
Dim Shared regDE As Integer

' Z80 Flags
Dim Shared fS As Integer
Dim Shared fZ As Integer
Dim Shared f5 As Integer
Dim Shared fH As Integer
Dim Shared f3 As Integer
Dim Shared fPV As Integer
Dim Shared fN As Integer
Dim Shared fC As Integer

' Flag bit positions
Const F_C As Integer = 1
Const F_N As Integer = 2
Const F_PV As Integer = 4
Const F_3 As Integer = 8
Const F_H As Integer = 16
Const F_5 As Integer = 32
Const F_Z As Integer = 64
Const F_S As Integer = 128

' Alternate registers
Dim Shared regAF_ As Integer
Dim Shared regHL_ As Integer
Dim Shared regBC_ As Integer
Dim Shared regDE_ As Integer

' Index registers  - ID used as temp for ix/iy
Dim Shared regIX As Integer
Dim Shared regIY As Integer
Dim Shared regID As Integer

' Stack pointer and program counter
Dim Shared regSP As Integer
Dim Shared regPC As Integer

' emulacion del DISPLAY del MPF-1P con 15 leds por caracter
Dim Shared As Integer dp0,dp1,dp2 ' columnas (compartidas con teclado)
Dim Shared As Integer kp0,kp1 ' filas (leds)
Dim Shared As Integer leds(20,16) ' total de leds a iluminar=20*16
dim shared as integer cuadros=0 ' saltarse cuadros para temas de velocidad

' almacen de leds encendidos
dim shared s1 (40*50) as integer ptr
Dim shared s2 (40*50) as integer ptr
Dim shared s3 (40*50) as integer ptr
Dim shared s4 (40*50) as integer ptr
Dim shared s5 (40*50) as integer ptr
Dim shared s6 (40*50) as integer ptr
Dim shared s7 (40*50) as integer ptr
Dim shared s8 (40*50) as integer ptr
Dim shared s9 (40*50) as integer ptr
Dim shared s10(40*50) as integer ptr
Dim shared s11(40*50) as integer ptr
Dim shared s12(40*50) as integer ptr
Dim shared s13(40*50) as integer ptr
Dim shared s14(40*50) as integer ptr
Dim shared s15(40*50) as integer ptr
Dim shared s16(40*50) as integer ptr

' almacen de leds apagados
dim shared s1o (40*50) as integer ptr
Dim shared s2o (40*50) as integer ptr
Dim shared s3o (40*50) as integer ptr
Dim shared s4o (40*50) as integer ptr
Dim shared s5o (40*50) as integer ptr
Dim shared s6o (40*50) as integer ptr
Dim shared s7o (40*50) as integer ptr
Dim shared s8o (40*50) as integer ptr
Dim shared s9o (40*50) as integer ptr
Dim shared s10o(40*50) as integer ptr
Dim shared s11o(40*50) as integer ptr
Dim shared s12o(40*50) as integer ptr
Dim shared s13o(40*50) as integer ptr
Dim shared s14o(40*50) as integer ptr
Dim shared s15o(40*50) as integer ptr
Dim shared s16o(40*50) as integer ptr

' almacen de la BIOS de los caracteres del monitor
dim shared caracteres(2048) as ubyte ' 256 caracteres de 8 bytes x 8bits