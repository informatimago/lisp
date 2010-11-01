Option Explicit

Dim Cuenta As New clsCuentaBancaria
Dim Nif As New clsNIF

Private Sub cmdCalcularLetra_Click()
  Nif.DarFormato = False
  Nif = txtNIF
  If Nif.Err Then
    txtNIF = "Error"
   Else
    txtNIF = Nif
  End If
  'If Nif.Validar(txtNIF) Then
  '  txtNIF = "Error"
  ' Else
  '  txtNIF = Nif
  'End If
End Sub

Private Sub cmdSalir_Click()
  Unload Me
End Sub

Private Sub cmdValidar_Click()
  Cuenta.DarFormato = False
  Cuenta = txtCuenta
  If Cuenta.Err Then
    txtCuenta = "Error"
   Else
    txtCuenta = Cuenta
  End If
  'If Cuenta.Validar(txtCuenta) Then
  '  txtCuenta = "Error"
  ' Else
  '  txtCuenta = Cuenta
  'End If
End Sub

La Clase clsCuentaBancaria.cls

VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsCuentaBancaria"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'****************************************
' (c) Carlos Augusto Barreira Díaz
' Fecha Creación: 03/06/97
'
' Finalidad:  Proporcionar una clase para validar números de Cuenta Bancario.
'
' Premisas y efectos: Su funcinamiento es totalmente autonomo.
'
' Funcionamiento: Hay dos formas de usar esta clase:
' 1.- Asignar a la Propiedad Cuenta (Que es la propiedad predeterminada)
' un valor de tipo string (es igual el formato ya que elimina todo lo que no
' sean numeros) y luego comprobar la propiedad Err.
' 2.- Usar la función Validar pasandole el nº de cuenta en un string y opcionalmente
' si se quiere pasar un boolean para dar formato al nº de cuenta (por defecto es true).
' Esta función devuelve el valor de la propiedad Err.
'
'**************************************

Option Explicit

Private Const strSep = "-"

Private mstrCuenta As String
Private mblnErr As Boolean
Private mblnDarFormato As Boolean
Private mintPesos(1 To 10) As Integer
Private strText1 As String, strBanco As String, strOficina As String, strCuenta As String, strDC1 As String, strDC2 As String
Private strDC1Bueno As String, strDC2Bueno As String

Public Property Get Err() As Boolean
  Err = mblnErr
End Property

Public Property Get DarFormato() As Boolean
Attribute DarFormato.VB_Description = "Si es True (Valor por defecto) se da formato al nº de cuenta despues de validar correctamente el nº."
  DarFormato = mblnDarFormato
End Property

Public Property Let DarFormato(blnDarFormato As Boolean)
  mblnDarFormato = blnDarFormato
End Property

Public Property Let Cuenta(ByVal vData As String)
  mstrCuenta = vData
  Validar_Todo
End Property

Public Property Get Cuenta() As String
Attribute Cuenta.VB_UserMemId = 0
  Cuenta = mstrCuenta
End Property

Public Function Validar(strCuentaBanco As String, Optional blnDarFormato As Boolean = True) As Boolean
  mstrCuenta = strCuentaBanco
  mblnDarFormato = blnDarFormato
  Validar_Todo
  Validar = mblnErr
  strCuentaBanco = mstrCuenta
End Function

Private Sub Class_Initialize()
  mstrCuenta = ""
  mblnErr = True: mblnDarFormato = True
  mintPesos(1) = 6: mintPesos(2) = 3: mintPesos(3) = 7: mintPesos(4) = 9: mintPesos(5) = 10
  mintPesos(6) = 5: mintPesos(7) = 8: mintPesos(8) = 4: mintPesos(9) = 2: mintPesos(10) = 1
End Sub

Private Sub Validar_Todo()
  Dim i As Integer
  mblnErr = False
  strText1 = "" 'Eliminar todo menos los numeros
  For i = 1 To Len(mstrCuenta)
    If IsNumeric(Mid$(mstrCuenta, i, 1)) Then
      strText1 = strText1 + Mid$(mstrCuenta, i, 1)
    End If
  Next
  If Len(strText1) <> 20 Then
    mblnErr = True
   Else
    strBanco = Mid(strText1, 1, 4)
    strOficina = Mid(strText1, 5, 4)
    strDC1 = Mid(strText1, 9, 1)
    strDC2 = Mid(strText1, 10, 1)
    strCuenta = Mid(strText1, 11, 10)
    If Not Validar_cuenta(strBanco & strOficina, strDC1, False, strDC1Bueno) Then
     mblnErr = True
    ElseIf Not Validar_cuenta(strCuenta, strDC2, True, strDC2Bueno) Then
     mblnErr = True
    ElseIf mblnDarFormato Then
      mstrCuenta = strBanco & strSep & strOficina & strSep & strDC1 & strDC2 & strSep & strCuenta
    End If
  End If
End Sub

Private Function Validar_cuenta(ByVal num As String, ByVal cc As String, ByVal Cuenta As Boolean, Optional strDcCorrecto As String) As Boolean
  Dim contpesos As Integer, contnum As Integer, suma As Long, resto As Integer, ccc As Integer

  If Cuenta Then
    contpesos = 10 'se paso un nº de cuenta
   Else
    contpesos = 8 ' se paso el nº de banco y oficina
  End If

  For contnum = 1 To Len(num)
    suma = suma + (mintPesos(contpesos) * CLng(Mid(num, contnum, 1)))
    contpesos = contpesos - 1
  Next
  resto = suma Mod 11
  ccc = 11 - resto
  If ccc = 10 Then ccc = 1
  If ccc = 11 Then ccc = 0
  If ccc = CInt(cc) Then
    Validar_cuenta = True
   Else
    Validar_cuenta = False
  End If
  If Not IsMissing(strDcCorrecto) Then
    strDcCorrecto = CStr(ccc)
  End If
End Function

La Clase: clsNIF.cls

VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsNIF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'****************************************
' (c) Carlos Augusto Barreira Díaz
' Fecha Creación: 03/06/97
'
' Finalidad:  Proporcionar una clase para validar y calcular letra del NIF.
'
' Premisas y efectos: Su funcinamiento es totalmente autonomo.
'
' Funcionamiento: La clase puede validar y calcular la letra del nif para
' ello asume que si la ultima letra del string que se le pasa es una letra valida
' lo que se quiere es validar el nif y si no lo que se quiere es calcular el NIF
' el resto de caracteres solo son significativos si son numericos. Así pues el
' funcionamiento es de dos formas:
' 1.- Asignar a la Propiedad NIF (Que es la propiedad predeterminada)
' un valor de tipo string (es igual el formato ya que elimina todo lo que no
' sean numeros) y luego comprobar la propiedad Err.
' 2.- Usar la función Validar pasandole el nº de NIF en un string y opcionalmente
' si se quiere dar formato al nº de cuenta (por defecto es true). Esta función
'  devuelve el valor de la propiedad Err.
' En ambos casos la propiedad NIF contiene el NIF resultado segun los calculos
' o formato indicados.
'**************************************

Option Explicit

Private Const strLetras = "TRWAGMYFPDXBNJZSQVHLCKE"
Private Const strSep = "-"

Private mstrNIF As String 'copia local
Private mstrLetra As String, mstrLetraBuena As String
Private lngNIF As Long
Private blnErr As Boolean, blnFormat As Boolean
Public Property Let DarFormato(blnFor As Boolean)
  blnFormat = blnFor
End Property

Public Property Get DarFormato() As Boolean
  DarFormato = blnFormat
End Property

Public Property Get Err() As Boolean
  Err = blnErr
End Property

Public Property Get Letra() As String
  Letra = mstrLetra
End Property

Public Property Let Nif(ByVal vData As String)
  mstrNIF = vData
  Validar_NIF
End Property

Public Property Get Nif() As String
Attribute Nif.VB_UserMemId = 0
  Nif = mstrNIF
End Property

Public Function Validar(Nif As String, Optional Format As Boolean = True) As Boolean
  mstrNIF = Nif
  blnFormat = Format
  Validar_NIF
  Validar = Err
  Nif = mstrNIF
End Function

Private Sub Validar_NIF()
  Dim strN As String
  
  blnErr = False
  strN = Dejar_Numeros(mstrNIF)
  If strN = "" Then
    blnErr = True
    mstrLetraBuena = "" ': mstrNIF = ""
   Else
    lngNIF = CLng(strN)
    mstrLetra = UCase(Mid(mstrNIF, Len(mstrNIF), 1))
    mstrLetraBuena = CalcularLetra
    If InStr(strLetras, mstrLetra) Then
      'se desea validar
      If mstrLetra <> mstrLetraBuena Then
        blnErr = True ': mstrNIF = ""
      End If
     Else
      mstrNIF = mstrNIF + mstrLetraBuena
    End If
    'si no se produjo error
    If (Not blnErr) And blnFormat Then
      mstrNIF = Format(lngNIF, "00,000,000") & strSep & mstrLetraBuena
    End If
  End If
End Sub

Private Function CalcularLetra() As String
  CalcularLetra = Mid(strLetras, 1 + (lngNIF Mod 23), 1)
End Function

Private Function Dejar_Numeros(strN As String) As String
  Dim i As Integer
  For i = 1 To Len(strN)
    If IsNumeric(Mid$(strN, i, 1)) Then
      Dejar_Numeros = Dejar_Numeros + Mid$(strN, i, 1)
    End If
  Next
End Function

Private Sub Class_Initialize()
  blnFormat = True
End Sub
