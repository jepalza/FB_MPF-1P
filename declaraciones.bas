' Emulador Z80 de Spectrum 
' retocado por Jepalza para que funcione en FreeBasic

'' boleanas (apagadas por ahora, por que FreeBasic ya las incorpora en la 1.0)
'Dim Shared False As Integer = 0
'Dim Shared True  As Integer = -1

' declares a llamar desde el modulo principal
Declare Function interrupt() As Integer
Declare Sub      Refresco_INTR(t As Integer) ' para instruccion LOAD_R
Declare Sub      execute()
Declare Sub      Z80Reset()

' Puertos entrada/salida (a escribir para cada emulacion)
Declare Function inb(port As integer) As Integer
Declare Sub     outb(port As integer, outbyte As Integer)

' Inicializaciones (tablas, que se llaman dentro de Z80Reset)
Declare Sub InitParity()
Declare Sub Divisores()

' solo para depuracion: hace que se pare en un RegPC
dim shared BreakPoint as Integer


' instrucciones
Declare Sub      adc_a(b As Integer)
Declare Function adc16(a As integer, b As integer) As Integer
Declare Sub      add_a(b As Integer)
Declare Function add16(a As integer, b As integer) As Integer
Declare Sub      and_a(b As Integer)
Declare Sub      ccf()
Declare Sub      cp_a(b As Integer)
Declare Sub      cpl_a()
Declare Sub      daa_a()
Declare Function dec16(a As integer) As Integer
Declare Sub      ex_af_af()
Declare Sub      exx()
Declare Function getAF() As Integer
Declare Function getBC() As Integer
Declare Function getD() As Integer
Declare Function getE() As Integer
Declare Function getF() As Integer
Declare Function getH() As Integer
Declare Function getIDH() As Integer
Declare Function getIDL() As Integer
Declare Function getL() As Integer
Declare Function id_d() As Integer
Declare Sub      ld_a_i()
Declare Sub      ld_a_r()
Declare Sub      neg_a()
Declare Sub      rld_a()
Declare Sub      setIDH(byteval As Integer)
Declare Sub      setIDL(byteval As Integer)
Declare Function in_bc() As Integer
Declare Function inc8(ByVal ans As integer) As Integer
Declare Function dec8(ByVal ans As integer) As Integer
Declare Sub      or_a(b As Integer)
Declare Sub      poppc()
Declare Function popw() As Integer
Declare Sub      pushpc()
Declare Sub      pushw(word As Integer)
Declare Function rl(ans As integer) As Integer
Declare Sub      rl_a()
Declare Function rlc(ans As integer) As Integer
Declare Sub      rlc_a()
Declare Function rr(ans As integer) As Integer
Declare Sub      rr_a()
Declare Function rrc(ans As integer) As Integer
Declare Sub      rrd_a()
Declare Sub      rrc_a()
Declare Sub      sbc_a(b As Integer)
Declare Function sbc16(a As integer, b As integer) As Integer
Declare Sub      scf()
Declare Sub      setAF(v As Integer)
Declare Sub      setBC(nn As Integer)
Declare Function qdec8(a As integer) As Integer
Declare Function inc16(a As integer) As Integer
Declare Sub      setD(l As Integer)
Declare Sub      setE(l As Integer)
Declare Sub      setF(b As Integer)
Declare Sub      setH(l As Integer)
Declare Sub      setL(l As Integer)
Declare Function sla(ByVal ans As integer) As Integer
Declare Function sls(ByVal ans As integer) As Integer
Declare Function sra(ByVal ans As integer) As Integer
Declare Function srl(ByVal ans As integer) As Integer
Declare Sub      sub_a(b As Integer)
Declare Sub      xor_a(b As Integer)



' varias
Declare Sub      bitv(b As integer, r As Integer)
Declare Function bitResv(bitvalor As integer, valor As integer) As Integer
Declare Function bitSetv(bitvalor As integer, valor As integer) As Integer
Declare Function execute_cb() As Integer
Declare Function execute_ed() As Integer
Declare Sub      execute_id_cb(op As integer, ByVal z As Integer)
Declare Function execute_id() As Integer
Declare Function nxtpcw() As Integer
Declare Function nxtpcb() As Integer

' Hardware
Declare Sub ResetKeyboard()
Declare function LeeTeclado(columna As integer) As integer
Declare Sub ponpantalla()
declare sub creadisplay() ' crea los leds
declare sub leecaracteres() ' lee la BIOS de las letras del monitor
declare sub PrintMPF(x as integer,y as integer,s as string) ' imprime los caracteres en fuentes MPF
declare sub printer(c As integer) ' imprime lel buffer de la impresora

' llamadas a memoria
Declare Sub      pokeb(addr As integer, Dato As Integer)
Declare Sub      pokew(addr As integer, word As Integer)

Declare Function peekb(addr As integer) As Integer
Declare Function peekw(addr As integer) As Integer



' ponemos textos en pantalla con pausa de modo rapido
Sub msg(texto As String, pausa As Integer)
	Print texto
	ScreenCopy
	If pausa Then sleep
End Sub