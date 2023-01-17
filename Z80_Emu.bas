' incluye definiciones de directorios
#include "dir.bi"

' inclusiones para las definiciones de teclado (comando multikey)
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif

' si queremos emulacion grafica completa, con mascara y todo. sino, solo es texto y monitor texto
#Define grafico 1

' declaraciones
#Include "declaraciones.bas"

' variables
#Include "variables.bas"

' rutinas de manejo de memoria (PEEK, POKE, IRQ)
#Include "memoria.bas"

' incluimos el nucleo emulador Z80
#include "Z80.bas"

' incluimos el nucleo Spectrum
#include "hard.bas"

' modulo de display
#include "leds.bas"


Sub LoadROM(sROMFile As String, inicio As Integer, tamano As Integer)
    Dim hFile As integer, sROM As String, lCounter As Integer

    hFile = Freefile
    Open sROMFile For Binary As hFile
        sROM = Input(tamano, #hFile) ' leemos de golpe todo el fichero
    Close hFile
    
    ' lo metemos en RAM en la dir "inicio"
    For lCounter = 1 To tamano
        RAM(inicio+(lCounter - 1)) = Asc(Mid(sROM, lCounter, 1))
    Next lCounter

End Sub



'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    ' maximo error de una interrupcion antes de "frenar" al PC, en milisegundos
    InterruptDelay = 20
    
    ' PRINCIPAL: ARRANCAMOS LA PANTALLA Y LA EMULACION
    ' abrimos una de 800x600x4bpp con una copia oculta para dibujar
    If grafico Then
	    Screenres 1275,574,8',,2 'habilitar las dos paginas si queremos dibujar sin parpadeos
	 Else
	 	 Screenres 640,480,8 ' para modos solo texto (depuracion)
    End If


    ResetKeyboard
    z80reset
    creadisplay
    leecaracteres ' BIOS de los caracteres del monitor
        
    ' 69888 tstates per interrupt (3.50000 MHz)
    ' se calcula como MHZ/50hz (o 60hz si es pal)
    ' ejemplo: 3.5mhz=3500000/50=70000 aprox. 69888 (mas real)
    TotalStates = 35000' 100000 para depuracion solo (ultrarapido)
    TiempoReal = Timer()
    
    ' zona de ram que desamos ver en pantalla para depurar
    verram=&hfe98

	 ' depuracion solo
	 'Open "pp.txt" For Output As 1
	 
    ' habilitamos la pagina 1 para dibujar oculto, la pagina 0 para la visible
    ' sino esta habilitado al crear el SCREEN , no afecta, y siempre es visible
    Screenset 1,0 

    MAXROM=&h4000 ' para que la rutina POKE no grabe en rom por accidente
    
    loadrom "roms\MPF1P.bin",&h0000,&h2000 ' 8k de rom monitor, MIO
    'loadrom "roms\MPF1P_alternativo.bin",&h0000,&h2000 ' 8k de rom monitor, alternativo, NO MIO
    LoadROM "roms\MPF1P-BASIC.bin",&h2000,&h2000 ' 8k de rom BASIC (el VIDEO da errores!!! se ve mal!!)
    'LoadROM "roms\MPF1P-FORTH.bin",&h2000,&h2000 ' 8k de rom FORTH (NO USA EL VIDEO, solo display!!!)
    LoadROM "roms\DEMOS.bin",&hB000,&h1000 ' 4k demos (semaforo, ....)
    
    ' el MPF-1P mira en la FEF3
    ' si es 1, existe y esta PRT ON, si es 0, existe pero no esta activa (PRT OFF) o no existe
    ' nota, no se si es cierto esto, por que si ejecuto G 195 (PRT ON/OFF) pone FF al apagar y 00 al encender!!
    LoadROM "roms\PRT.bin",&h6000,&h1000 ' impresora 4k
 	 	    
    ' monitor de video, si esta activo, se muestran los comandos en el 
    ' simulador de video, pero entonces, algunos comandos (como "R") ya no
    ' se muestran en el display.
    ' importante hacer notar, que si la version de la ROM de video es vieja, 
    ' DEBERIA ir en la 0800, pero si es la nueva 2.0 (la que tengo yo), va en la A000
    ' el MPF-1P comprueba si el BYTE de la A000 es A5, en cuyo caso, se activa el modo TV
    LoadROM "roms\video\VIOv20.bin",&hA000,&h0800 ' 2k de rom VIDEO MONITOR OUTPUT, version NUEVA (ojo, importante)
    ' ademas, emplea una BIOS "viochar.bin" de caracteres graficos que se lee en leecaracteres()
       
    ' mascara grafica del teclado y display   
    If grafico Then
    	Dim tecladobmp As Any Ptr = ImageCreate( 950, 574 )
		BLoad "teclado\teclado.bmp", tecladobmp
		Put (0,0), tecladobmp
		ImageDestroy( tecladobmp )
    End If 	
    
	   ' colores para los led
		palette 0,rgb(0,0,0) ' fondo
		palette 7,rgb(0,10,0) ' lineas apagadas
		palette 2,rgb(0,32,0) ' borde lineas encendidas

		palette 10,rgb(0,40,0) ' interior lineas encendidas
	 
	 ' linea de separacion del monitor de video
	 Line (950,281)-Step(325,8),RGB(63,63,63),bf
	 ' cuadro libre, sin utilidad aun
	 Line (950,290)-Step(325,300),RGB(30,30,30),bf

   
    ' // Begin the Z80 execution loop, this drives the whole emulation
    sleep 100 ' necesario para que le de tiempo al teclado a vaciarse
    execute