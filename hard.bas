' rutinas de emulacion hardware del Spectrum 48k unicamente

Sub Z80Reset()
    regPC = 0
    regSP = 65535
    regA = 0
    setF 0
    setBC 0
    regDE = 0
    regHL = 0
    
    exx
    ex_af_af
    
    regA = 0
    setF 0
    setBC 0
    regDE = 0
    regHL = 0
    
    regIX = 0
    regIY = 0
    intR = 128
    intRTemp = 0
    
    intI = 0
    intIFF1 = False
    intIFF2 = False
    intIM = 0

    InitParity
    Divisores
    
End Sub

Sub resetKeyboard()
    do : loop while inkey<>""
End Sub

' FORMATO DE LECTURA DEL TECLADO:
' SE COMPONE DE 20 COLUMNAS POR 3 FILAS
' EL MPF ESCANEA CADA COLUMNA DE 1 A 20, Y VERIFICA SI UN BIT(0-2) SE ACTIVA
'  01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20
' |------------------------------------------------------------
'1| 1  2  3  4  5  6  7  8  9  0  Q  W  E  R  T  Y  U  I  O  P
'2| A  S  D  F  G  H  J  K  L  :  Z  X  C  V  B  N  M  ,  .  ?
'3| Es Ci Cd Ca Cb Cr
'  --> Es= ESPACIO, Ci=IZQUIERDA, Cd=DERECHA, Ca=ABAJO, Cb=ARRIBA, Cr=INTRO 
'
' RS=RETROCESO (BORRADO)
' SHIFT (PC4 del puerto C del 8255)
' CONTROL (PC5 del puerto C del 8255)
' BREAK=BIT(4) PUERTO C 8255
function LeeTeclado(columna As integer) As Integer
    Dim tecla As Integer
    tecla=0

    ' cada vez que entramos, borramos las teclas
    resetKeyboard
    
    ' escape
    if MultiKey(SC_ESCAPE)  Then tecla=27: Close:End
    if MultiKey(SC_F2)  Then z80reset():exit function

	Select Case columna
		Case 1
			' COLUMNA 01
			If MultiKey(SC_1) then tecla=1
			if MultiKey(SC_A) then tecla=2
			if MultiKey(SC_SPACE) then tecla=3
		Case 2
			' COLUMNA 02    
			if MultiKey(SC_2) then tecla=1
			if MultiKey(SC_S) then tecla=2
			if MultiKey(SC_LEFT) then tecla=3
		Case 3
			' COLUMNA 03    
			if MultiKey(SC_3) then tecla=1
			if MultiKey(SC_D) then tecla=2
			if MultiKey(SC_RIGHT) then tecla=3
		Case 4
			' COLUMNA 04    
			if MultiKey(SC_4) then tecla=1
			if MultiKey(SC_F) then tecla=2
			if MultiKey(SC_DOWN) then tecla=3
		Case 5
			' COLUMNA 05   
			if MultiKey(SC_5) then tecla=1
			if MultiKey(SC_G) then tecla=2
			if MultiKey(SC_UP) then tecla=3
		Case 6
			' COLUMNA 06    
			if MultiKey(SC_6) then tecla=1
			if MultiKey(SC_H) then tecla=2
			if MultiKey(SC_ENTER) then tecla=3
		Case 7
			' COLUMNA 07   
			if MultiKey(SC_7) then tecla=1
			if MultiKey(SC_J) then tecla=2
			if MultiKey(SC_F4) then tecla=3
		Case 8
			' COLUMNA 08   
			if MultiKey(SC_8) then tecla=1
			if MultiKey(SC_K) then tecla=2
		Case 9
			' COLUMNA 09    
			if MultiKey(SC_9) then tecla=1
			if MultiKey(SC_L) then tecla=2
		Case 10
			' COLUMNA 10
			if MultiKey(SC_0) then tecla=1
			if MultiKey(SC_SLASH) then tecla=2
		Case 11
			' COLUMNA 11    
			if MultiKey(SC_Q) then tecla=1
			if MultiKey(SC_Z) then tecla=2
		Case 12
			' COLUMNA 12    
			if MultiKey(SC_W) then tecla=1
			if MultiKey(SC_X) then tecla=2
		Case 13
			' COLUMNA 13
			if MultiKey(SC_E) then tecla=1
			if MultiKey(SC_C) then tecla=2
		Case 14
			' COLUMNA 14
			if MultiKey(SC_R) then tecla=1
			if MultiKey(SC_V) then tecla=2
		Case 15
			' COLUMNA 15
			if MultiKey(SC_T) then tecla=1
			if MultiKey(SC_B) then tecla=2
		Case 16
			' COLUMNA 16
			if MultiKey(SC_Y) then tecla=1
			if MultiKey(SC_N) then tecla=2
		Case 17
			' COLUMNA 17
			if MultiKey(SC_U) then tecla=1
			if MultiKey(SC_M) then tecla=2
		Case 18
			' COLUMNA 18
			if MultiKey(SC_I) then tecla=1
			if MultiKey(SC_COMMA) then tecla=2
		Case 19
			' COLUMNA 19
			if MultiKey(SC_O) then tecla=1
			if MultiKey(SC_PERIOD) then tecla=2
		Case 20
			' COLUMNA 20
			if MultiKey(SC_P) then tecla=1
			if MultiKey(SC_SEMICOLON) then tecla=2
	End Select

   ' rotamos el bit activo de cada fila 0-1-2
   If tecla Then tecla=1 Shl (tecla-1)
   
 	Return tecla Xor 255
End Function


Sub led(c As integer,f As Integer, activo As Integer)
	Dim As Integer g,h,i,x,y

	x=80+((c-1)*40-1)
	y=35
	
	if activo=1 then ' leds ON
	   ' horizontales
		If f= 1 Then put (x,y), @s1 (0),trans
		If f= 2 Then put (x,y), @s2 (0),trans
		If f= 3 Then put (x,y), @s3 (0),trans
		If f= 4 Then put (x,y), @s4 (0),trans
		If f= 5 Then put (x,y), @s5 (0),trans
		If f= 6 Then put (x,y), @s6 (0),trans
		If f= 7 Then put (x,y), @s7 (0),trans
		If f= 8 Then put (x,y), @s8 (0),trans
		' diagonales
		If f= 9 Then put (x,y), @s9 (0),trans
		If f=10 Then put (x,y), @s10(0),trans
		If f=11 Then put (x,y), @s11(0),trans
		If f=12 Then put (x,y), @s12(0),trans
		If f=13 Then put (x,y), @s13(0),trans
		If f=14 Then put (x,y), @s14(0),trans
		' puntos
		If f=15 Then put (x,y), @s15(0),trans
		if f=16 Then put (x,y), @s16(0),trans
	else  ' leds OFF
		' horizontales
		If f= 1 Then put (x,y), @s1o (0),trans
		If f= 2 Then put (x,y), @s2o (0),trans
		If f= 3 Then put (x,y), @s3o (0),trans
		If f= 4 Then put (x,y), @s4o (0),trans
		If f= 5 Then put (x,y), @s5o (0),trans
		If f= 6 Then put (x,y), @s6o (0),trans
		If f= 7 Then put (x,y), @s7o (0),trans
		If f= 8 Then put (x,y), @s8o (0),trans
		' diagonales
		If f= 9 Then put (x,y), @s9o (0),trans
		If f=10 Then put (x,y), @s10o(0),trans
		If f=11 Then put (x,y), @s11o(0),trans
		If f=12 Then put (x,y), @s12o(0),trans
		If f=13 Then put (x,y), @s13o(0),trans
		If f=14 Then put (x,y), @s14o(0),trans
		' puntos
		If f=15 Then put (x,y), @s15o(0),trans
		if f=16 Then put (x,y), @s16o(0),trans
	endif
			
End Sub

sub leecaracteres()
  dim reg as integer
  dim sc as string
  
	reg=1

	sc=" "
	Open "roms\video\VIOchar.bin" For Binary Access Read As 1
	While Not Eof(1)
		Get #1, reg, sc
	   caracteres(reg-1)=asc(sc)
		reg+=1 ' mas lento de uno en uno, pero menos liante de programar
	wend
	close 1
 
end sub

sub PrintMPF(x as integer, y as integer,s as string)
	dim as integer a,b,c,i,f,g

	a=asc(s)
	i=0:if a>127 then a-=128:i=1 ' mayores de 128 son los mismos, pero invertidos
	for f=0 to 13
		b=caracteres((a*16)+f)
		for g=0 to 7
			c=(1 and (b shr(7-g)))
			If c=0 Then c=4 ' usar esta linea para cambiar el color de fondo tambien
			if i then c=1-c ' si es invertido, la I=1 hace la inversion
			pset ((y*8)+g,(x*14)+f),c*10 ' conseguimos un color verde monitor (el 10, el mismo que los leds)
		next
	next

end sub


sub ponpantalla()

    DIM i as integer, m as Integer, a as Integer, n As integer
    Dim x As integer, y As Integer, h as integer,g as integer
    Dim ff As integer
   
     printer(5)
   
    ' juntamos las lineas de datos para formar las 20 columnas
    'pruebas .. h=(dp0 Shl 12)+(dp1 Shl 4)+(dp2 And &h0F)
    h=dp2 Shl 16+dp1 Shl 8+dp0
    ' y lo mismo con las 16 filas (16 leds)
    'pruebas .. g=((kp1 And (64+128)) Shl 14)+((kp1 And 16) Shl 13)+((kp1 And 32) Shl 12)+((kp1 And &h0F) Shl 8) + kp0
	 g=kp1 *256 + kp0

    If grafico=1 Then
	    ' ahora dibujamos la matriz activa de leds en base a filas*columnas encendidas
	    For x=1 To 20 ' 20 caracteres por 16 leds cada uno
	      For y=1 To 15
	      	leds(x,y)=IIf((h And (1 Shl (x-1))),0,1) and IIf((g And (1 Shl (y-1))),0,1)
				If IIf((h And (1 Shl (x-1))),0,1) Then led(x,y,leds(x,y))
	      next
	    next 
    'Else
    '   para ver la linea donde escribe el mpfa, pero solo si NO hay video
	 '   FF=verram
	 '   For i=0 To 19
	 '   		a=RAM(&hFF18+i)
	 '   		Locate 35,i+1+40
	 '   		If ff<&h10000 Then Print IIf (a>31,Chr(a),".")
	 '   Next
    End If
   
   
  ' el modo grafico es lento, por lo tanto
  'saltamos cuadros para velocidad
  cuadros+=1
  If cuadros<32 then exit sub
  cuadros=0


    ' ESTA RUTINA "EMULA" LA SALIDA VIDEO-OUTPUT SI LA ROM "VIO-V20.BIN"
    ' SOLO SI ESTA INSTALADA LA ROM EN LA DIRECCION &HA000
    ' (pantalla es de 40x20 caracteres)
    FF=&h4000
    color 200,0
    For i=0 To 19
    	For m=0 To 39
    		a=RAM(FF)
    		PrintMPF ( i , m+IIf(grafico,119,0) , Chr(a) )
    		FF+=1 
    	Next
    Next

	'Locate 1,60:Print "VIDEO OUTPUT : 0x";Hex(verram,4)
 
 	Color 3
 
 	' zona de depuracion
 	#If 0 ' 0=anulado, 1=activo
	    If MultiKey(SC_PAGEDOWN) then verram+=64
	    if MultiKey(SC_PAGEUP) then verram-=64
	    ' ponemos una zona de RAM primero en HEXA
	    FF=verram
	    Locate 40,25:Print Hex(verram,4)
	    If FF<0 Then FF=0
	    If FF>&hffff Then ff=&Hffff
	    For i=0 To 14
	    	n=1
	    	For m=0 To 47 Step 3
	    		a=RAM(FF)
	    		Locate i+40,m+33
	    		If ff<&h10000 Then Print Hex(a,2) Else Print "##"
	    		Locate i+40,n:n+=1
	    		If ff<&h10000 Then Print IIf (a>31 And a<>255,Chr(a),".") Else Print "#"
	    		FF+=1
	    	Next
	    Next
	#EndIf

    ' una vez dibujada en oculto, volcamos a la visible
    ScreenCopy
End Sub


' control de puertos I/O
Function inb(port As integer) as Integer
    Dim ret As integer
    Dim x As integer,h as Integer
    
    ret=255
    
    'Print #1,"   inp:";Hex(port,2)
    
    ' nota: el puerto C del PIA-1 (8255) es de salida para los bits 0,1 y 2 (teclado) 
    ' pero de entrada para los bits 3 y 4 (teclas especiales SHIFT y CONTROL)
    If (port And &HFF) = &H92 Then
      h=dp2 Shl 16+dp1 Shl 8+dp0
      'Locate 24,10:Print Bin(h,24)

	    For x=1 To 20 ' 20 columnas de teclas de 3 filas cada una
	      'columkeys(x)=IIf((h And (1 Shl (x-1))),0,1) and IIf((g And (1 Shl (y-1))),0,1)
			If (h And (1 Shl (x-1)))=0 Then ret=LeeTeclado(x)
	    next 
	    
      'ram(&hff04) ' inicio de buffer 40c para el teclado
      'ram(&hff2c) ' inicio de buffer 82c para el display
    End If

    ' puertos de lectura solo para las teclas SHIFT y CONTROL (bits 4 y 5)
    If (port And &HFF) = &H82 then
    	if MultiKey(SC_LSHIFT)  then ret=1 ' shift bit 4
    	if MultiKey(SC_CONTROL) then ret=2 ' control bit 5
    end if
    
    return ret
    
End Function

Sub outb(port As integer, outbyte As Integer)

	 
    ' puertos de filas comunes al LCD y al teclado
    If (port And &hFF) = &h80 Then
   	dp0=outbyte And &hff
   	Exit sub
    End If
    If (port And &hFF) = &h81 Then
   	dp1=outbyte And &hff
   	Exit Sub
    End If   
    If (port And &hFF) = &h82 Then
   	dp2=outbyte And &hff
   	Exit Sub
    End If
    ' puertos de LEDS del LCD
    If (port And &hFF) = &h90 Then
   	kp0=outbyte And &hff
   	Exit Sub
    End If
    If (port And &hFF) = &h91 Then
   	kp1=outbyte And &hff
   	Exit sub
    End If
    
	 'Print #1,"out:";Hex(port,2);" - ";Hex(outbyte,2)
	 
	 ' 82, 83, 93, F0, F1 incognitas
    'If (port And &hFF) = &hF0 Then
   	'Print Chr(outbyte And &hff);
   	'Exit sub
    'End If
    
    'If (port And &hFF) = &hF1 Then
   	'Print chr(outbyte And &hff);
   	'Exit sub
    'End If   
	 
    ' salir del emulador. lo ponemos aqui, por que si se cuelga, no sale. Temporal para siempre
    if MultiKey(SC_ESCAPE)  Then end
    
    
End Sub






''''''''''''''''''''''''''''''''''''
'''''''  MANEJO DE LAS IRQ '''''''''
''''''''''''''''''''''''''''''''''''
Function interrupt() As Integer
	Dim lcounter as integer ' tiempo a dormir el PC dentro de INTERRUPT para frenarlo a la velocidad Z80 real
	Dim lsleep as integer ' recoge el valor del TIMER(), tiempo trascurrido entre dos llamadas a INTERRUPT
   Dim tecla As Integer
     
   interruptCounter = interruptCounter + 1  
   If (interruptCounter Mod 16)=0 Then
        ' ejecuciones cada 16 veces (1/2 segundo) de cada IRQ
        ' para elementos que no requieren tanta actualizacion
        ' como un parpadeo en pantalla de un caracter, por ejemplo
        ' o la lectura de un teclado
   End If
        
    ' If it's a maskable interrupt
    If intIFF1 = False Then
        interrupt = 0
    Else
        Select Case intIM
        Case 0, 1
            pushpc
            intIFF1 = False
            intIFF2 = False
            regPC = 56
            interrupt = 13
        Case 2
            pushpc
            intIFF1 = False
            intIFF2 = False
            regPC = peekw((intI * 256) Or &HFF)
            interrupt = 19
        End Select
    End If
   
   ' miramos el tiempo transcurrido entre dos IRQ
    lSleep = Timer() - TiempoReal
    
    If lSleep = 0 Then lSleep = 1
    
    If lSleep < InterruptDelay + 1 Then     
        lCounter = InterruptDelay - lSleep - DelayOverage
        If lCounter < 0 Then
            DelayOverage = -lCounter
        Else
            DelayOverage = DelayOverage - ((InterruptDelay - lSleep) - lCounter)
            ' este sleep causa una pausa superalta y detiene el emul. estudiarlo
            ' EL SEGUNDO PARAMETRO DE SLEEP, IMPIDE QUE SE INTERRUMPA CON UNA TECLA
            If VelocidadReal then Sleep lCounter,1
        End If
    Else
        DelayOverage = DelayOverage + lSleep - InterruptDelay
        'Locate 20,20:Print delayoverage,InterruptDelay
        If DelayOverage > 48 Then DelayOverage = 48
    End If

    TiempoReal = Timer()
    
End Function

' invento MUY MAL HECHO para emular la impresora.
' funciona de PUTA pena, no muestra todo lo que deberia y muestra mas de lo que debe.
' lo dejo abandonado , queda para el futuro si me apetece
Sub printer(c As Integer)
	' zonas interesantes
	' 6A00 lleva la cabeza al incio, retorno de carro (izquierda)
	' 6A10 salto linea (line feed)
	' 6A30 salta dos lineas
	' 6A40 imprimer el buffer (direccion FF04 ?)
	' ff84 2c 34 de dos en ds
	
	Exit Sub ' por ahora, anulado
	
	Dim i As Integer
	Dim j As Integer
	Dim a As Integer
	Dim b As Integer
	Dim sa As String
	sa=""


   For i=0 To 19
    	a=RAM(&hFF04+i)
    	If c<4 Then Locate 38,10:print rnd:a=&h0d
    	If a<>0 Then 
    		If a=&h0d Or i=19 Then 
    			RAM(&hFF04+i)=0 ' borro el 0x0d para que no se repita
				for j=2 To 35
					For b=41 To 80
						a=Screen(j,b)
						Locate j-1,b
						Print Chr(a);
					Next
				Next
				Locate 35,41:Print Space(40);
				Locate 35,41
    			Print sa
    			Exit For
    		EndIf
    		sa=sa+IIf (a>1 And a<255,Chr(a),".")
    	EndIf
   Next

End Sub
