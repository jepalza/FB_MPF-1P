
Sub Divisores()
    ' es mas rapido recuperar un dato precalculado, que hacerlo en tiempo real 
    ' por eso, hacemos los calculos y los guardamos para futuras operaciones
    Dim f as integer
    For f = 0 To 81919
        Div4    (f) = (f And 65535) \ 4
        Div32   (f) = (f And 65535) \ 32
        Div256  (f) = (f And 65535) \ 256
        Div16384(f) = (f And 65535) \ 16384
    Next f
End Sub

' precalculamos una matriz para acelerar procesos de CPU 
Sub InitParity()
    Dim b as byte
    Dim f As Integer
    Dim l As Integer
    
    For f = 0 To 255
        l = True
        For b = 0 To 7
            If (f And (2 ^ b)) <> 0 Then l = Not l
        Next b
        Parity(f) = l
    Next f
End Sub


''''''''''''''''''''''''''''''''''''''
''''' control de la memoria RAM ''''''
''''''''''''''''''''''''''''''''''''''
Function peekb(addr As integer) As Integer
    addr=addr and &hffff  
    peekb = RAM(addr) and &HFF

    'if addr=&h893 Then deb=2:BreakPoint=1

	 ' llamada a PRINT ALL (&h6A40) (salida de linea BUFFER en la FF04 a la impresora)
    'If addr>=&h6000 And addr<=&h67FF Then printer() 
    If addr=&h6A00 Then printer(0) ' retorno de carro al inicio (CR=Carriadge Return)
    If addr=&h6A10 Then printer(1) ' retorno de carro bajando linea (LF=Line Feed)
    If addr=&h6A30 Then printer(2) ' sube dos lineas el carro
    If addr=&h6A40 Then printer(3) ' imprime el buffer en la ff04
                
    'if addr=&h25f then peekb=0
    'If addr=&h260 Then peekb=0
    if deb>1 then locate 1,40:msg "PeekB:"+Hex(addr)+" Dato:"+hex(RAM(addr))+"       ",0  
End Function

Function peekw(addr As integer) As Integer
    peekw = peekb(addr)+(256 * peekb(addr+1))
    if deb>1 then locate 2,40:msg "PeekW:"+hex(addr)+" Dato:"+Hex(RAM(addr)+(256*RAM(addr+1)))+"       ",0   
End Function

Sub pokeb(addr As integer, Dato As Integer)
    addr=addr and &hffff

    If addr < MAXROM Then
        ' estamos en ROM
        Exit Sub
    End If

    RAM(addr) = Dato And &hff
    if deb>1 then locate 3,40:msg "PokeB:"+hex(addr)+" Dato:"+Hex(RAM(addr))+"       ",0
    
End Sub

Sub pokew(addr As integer, word As Integer)
    addr=addr and &hffff
    word=word and &hffff

    pokeb addr, word And &HFF
    pokeb addr + 1, word Shr 8 'Div256((word And &HFF00))
    if deb>1 then locate 4,40:msg "PokeW:"+hex(addr)+" Dato:"+hex(RAM(addr)+(256*RAM(addr+1)))+"       ",0
End Sub

Sub poppc()
    regPC = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &HFFFF)
End Sub

Function popw() As Integer
    popw = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &HFFFF)
End Function

 Sub pushpc()
    regSP = (regSP - 2) And &HFFFF
    pokew regSP, regPC
End Sub

Sub pushw(word As Integer)
    regSP = (regSP - 2) And &HFFFF
    pokew regSP, word
End Sub
