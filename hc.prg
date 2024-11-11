/*
 * Harbour Commander (HC) Project
 * Copyright 2014 - 2024 Rafał Jopek
 * Website: https://harbour.pl
 *
 * This work has been migrated from my project, which was originally developed
 * in  the  Harbour  language. The code is inspired  by previous functions and
 * structures  that  were  implemented  in  that  project. This note serves to
 * highlight  the  development  history  and the source of inspiration for the
 * current project.
 *
 */

/* Keeping it tidy */
#pragma -w3
#pragma -es2

/* Optimizations */
#pragma -km+
#pragma -ko+

#include "hbsdl.ch"

/* Harbour */
#include "directry.ch"

// ---
#define _col                  1
#define _row                  2
#define _maxCol               3
#define _maxRow               4
#define _currentDir           5
#define _directory            6
#define _filesCount           7
#define _rowBar               8
#define _rowNo                9
#define _cmdLine             10
#define _cmdOutput           11
#define _cmdCol              12
#define _cmdColNo            13
#define _isFirstDirectory    14
#define _isSizeVisible       15
#define _isAttrVisible       16
#define _isDateVisible       17
#define _isTimeVisible       18

#define _elements            18
// ---
#define F_MODE               6
// ---

PROCEDURE Main()

   LOCAL lQuit := .F.

   LOCAL pSdl
   LOCAL pEvent

   LOCAL aLeftPanel
   LOCAL aRightPanel
   LOCAL aActivePanel

   LOCAL lVisiblePanels := .T.

   pSdl := sdl_CreateWindow( 830, 450, "Harbour Commander", "F1F1F1" )

   sdl_LoadFont( pSdl, "./font/9x18.pcf.gz", 18 )

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   aLeftPanel  := hc_init()
   aRightPanel := hc_init()

   Set( _SET_DATEFORMAT, "dd-mm-yyyy" )

   aLeftPanel  := hc_fetchList( aLeftPanel, hb_cwd() )
   aRightPanel := hc_fetchList( aRightPanel, hb_cwd() )

   aActivePanel := aLeftPanel

   DO WHILE( !lQuit )

      pEvent := sdl_WaitEvent()

      SWITCH( sdl_EventType( pEvent ) )

         CASE SDL_QUIT

            lQuit := .T.
            EXIT

         CASE SDL_WINDOWEVENT

            IF sdl_EventWindowEvent( pEvent ) == SDL_WINDOWEVENT_CLOSE
               lQuit := .T.
            ENDIF
            EXIT

         CASE SDL_KEYDOWN

            SWITCH( sdl_EventKeyKeysymSym( pEvent ) )

               CASE SDLK_ESCAPE
                  lQuit := .T.
                  EXIT

               CASE SDLK_TAB

                  IF lVisiblePanels
                     IF aActivePanel == aLeftPanel
                        aActivePanel := aRightPanel
                        aActivePanel[ _cmdLine ] := aLeftPanel[ _cmdLine ]
                        aActivePanel[ _cmdCol  ] := aLeftPanel[ _cmdCol ]
                        aLeftPanel[   _cmdLine ] := ""
                        aLeftPanel[   _cmdCol  ] := 0
                     ELSE
                        aActivePanel := aLeftPanel
                        aActivePanel[ _cmdLine ] := aRightPanel[ _cmdLine ]
                        aActivePanel[ _cmdCol  ] := aRightPanel[ _cmdCol ]
                        aRightPanel[  _cmdLine ] := ""
                        aRightPanel[  _cmdCol  ] := 0
                     ENDIF
                  ENDIF
                  EXIT

               OTHERWISE

                  IF sdl_EventKeyKeysymSym( pEvent ) == SDLK_o .AND. hb_BitAnd( sdl_GetModState(), KMOD_LCTRL ) != 0
                     lVisiblePanels := hc_togglePanels( lVisiblePanels )
                  ENDIF
                  EXIT

            ENDSWITCH

         OTHERWISE

      ENDSWITCH

      sdl_BeginDraw( pSdl )

         IF lVisiblePanels
            aLeftPanel := hc_resize( aLeftPanel, 0, 0, sdl_maxCol( pSdl ) / 2, sdl_maxRow( pSdl ) - 1 )
            aRightPanel := hc_resize( aRightPanel, sdl_maxCol( pSdl ) / 2, 0, sdl_maxCol( pSdl ) / 2, sdl_maxRow( pSdl ) - 1 )

            hc_drawPanel( pSdl, aActivePanel, aLeftPanel )
            hc_drawPanel( pSdl, aActivePanel, aRightPanel )
            sdl_setBackground( pSdl, "F1F1F1" )
         ELSE
            sdl_setBackground( pSdl, "000000" )
         ENDIF

      sdl_EndDraw( pSdl )

   ENDDO

RETURN

/* -------------------------------------------------------------------------
hc_init() --> aPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_init()

   LOCAL aPanel

   aPanel := Array( _elements )

   aPanel[ _col               ] :=  0
   aPanel[ _row               ] :=  0
   aPanel[ _maxCol            ] :=  0
   aPanel[ _maxRow            ] :=  0
   aPanel[ _currentDir        ] :=  ""
   aPanel[ _directory         ] :=  {}
   aPanel[ _filesCount        ] :=  0
   aPanel[ _rowBar            ] :=  1
   aPanel[ _rowNo             ] :=  0
   aPanel[ _cmdLine           ] :=  ""
   aPanel[ _cmdOutput         ] :=  ""
   aPanel[ _cmdCol            ] :=  0
   aPanel[ _cmdColNo          ] :=  0
   aPanel[ _isFirstDirectory  ] := .T.
   aPanel[ _isSizeVisible     ] := .T.
   aPanel[ _isAttrVisible     ] := .T.
   aPanel[ _isDateVisible     ] := .T.
   aPanel[ _isTimeVisible     ] := .T.

   RETURN aPanel

/* -------------------------------------------------------------------------
hc_fetchList( aSelectedPanel, cDir ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_fetchList( aSelectedPanel, cDir )

   LOCAL i, aTempFiles := {}

   // Ustawienie bieżącego katalogu
   aSelectedPanel[ _currentDir ] := hb_defaultValue( cDir, hb_cwd() )
   aSelectedPanel[ _directory ] := Directory( aSelectedPanel[ _currentDir ], "HSD" )

   // Dodawanie elementów do tymczasowej tablicy
   FOR i := 1 TO Len( aSelectedPanel[ _directory ] )
      AAdd( aTempFiles, aSelectedPanel[ _directory ][ i ] )
      // Dodaje wartość .T. na końcu każdego wpisu w aTempFiles
      AAdd( aTempFiles[ Len( aTempFiles ) ], .T. )
   NEXT

   aSelectedPanel[ _directory ] := aTempFiles
   aSelectedPanel[ _filesCount ] := Len( aTempFiles )

   // 1. Katalog ".." zawsze na początku
   ASort( aSelectedPanel[ _directory ],,, { | x | x[ F_NAME ] == ".." } )

   // 2. Katalog "." zaraz po ".." (nie jest drukowany w hc_drawPanel )
   ASORT( aSelectedPanel[ _directory ],,, { | x, y | ( x[ F_NAME ] == "." ) .AND. ( y[ F_NAME ] != ".." ) } )

   // 3. Zwykłe katalogi przed ukrytymi katalogami
   ASort( aSelectedPanel[ _directory ],,, { | x, y | "D" $ x[ F_ATTR ] .AND. !( "H" $ x[ F_ATTR ] ) .AND. ( "H" $ y[ F_ATTR ] ) } )

   // 4. Katalogi przed plikami (bez względu na ukrycie)
   ASort( aSelectedPanel[ _directory ],,, { | x, y | "D" $ x[ F_ATTR ] .AND. !( "D" $ y[ F_ATTR ] ) } )

RETURN aSelectedPanel

/* -------------------------------------------------------------------------
hc_resize( aSelectedPanel, nCol, nRow, nMaxCol, nMaxRow ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_resize( aSelectedPanel, nCol, nRow, nMaxCol, nMaxRow )

   aSelectedPanel[ _col    ] := nCol
   aSelectedPanel[ _row    ] := nRow
   aSelectedPanel[ _maxCol ] := nMaxCol
   aSelectedPanel[ _maxRow ] := nMaxRow

RETURN aSelectedPanel

/* -------------------------------------------------------------------------
hc_drawPanel( pSdl, aActivePanel, aSelectedPanel ) --> NIL
------------------------------------------------------------------------- */
STATIC PROCEDURE hc_drawPanel( pSdl, aActivePanel, aSelectedPanel )

   LOCAL i := 1
   LOCAL nRow
   LOCAL nLongestName := 4
   LOCAL nLongestSize
   LOCAL nLongestAttr
   LOCAL cPaddedString
   LOCAL cPaddedResult
   LOCAL cSelectedColor

   IF aActivePanel == aSelectedPanel
      sdl_drawBox( pSdl, aSelectedPanel[ _col ], aSelectedPanel[ _row ], aSelectedPanel[ _maxCol ], aSelectedPanel[ _maxRow ], BOX_DOUBLE, "F1F1F1/323232" )
   ELSE
      sdl_drawBox( pSdl, aSelectedPanel[ _col ], aSelectedPanel[ _row ], aSelectedPanel[ _maxCol ], aSelectedPanel[ _maxRow ], BOX_SINGLE, "F1F1F1/323232" )
   ENDIF

   nLongestName := MAX( nLongestName, hc_findLongestName( aSelectedPanel ) )
   nLongestSize := hc_findLongestSize( aSelectedPanel )
   nLongestAttr := hc_findLongestAttr( aSelectedPanel )

   i += aSelectedPanel[ _rowNo ]
   FOR nRow := aSelectedPanel[ _row ] + 1 TO aSelectedPanel[ _maxRow ] - 1

      IF i <= aSelectedPanel[ _filesCount ]

      // Pomijamy wyświetlanie bieżącego katalogu "."
      IF aSelectedPanel[ _directory ][ i ][ F_NAME ] == "."
         ++i
      ENDIF

      cPaddedString := hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr,;
            aSelectedPanel[ _directory ][ i ][ F_NAME ],;
            aSelectedPanel[ _directory ][ i ][ F_SIZE ],;
            aSelectedPanel[ _directory ][ i ][ F_DATE ],;
            aSelectedPanel[ _directory ][ i ][ F_TIME ],;
            aSelectedPanel[ _directory ][ i ][ F_ATTR ] )

         cPaddedResult := PadR( cPaddedString, aSelectedPanel[ _maxCol ] - 2 )

         IF aActivePanel == aSelectedPanel .AND. i == aSelectedPanel[ _rowBar ] + aSelectedPanel[ _rowNo ]

            IF !aSelectedPanel[ _directory ][ i ][ F_MODE ]
               cSelectedColor := "323232/FF4D4D"
            ELSE
               cSelectedColor := "323232/00FF00"
            ENDIF

         ELSE
            cSelectedColor := hc_selectColor( aSelectedPanel[ _directory ][ i ][ F_ATTR ], aSelectedPanel[ _directory ][ i ][ F_MODE ] )
         ENDIF

         sdl_drawFont( pSdl, aSelectedPanel[ _col ] + 1, nRow, cPaddedResult, cSelectedColor )

         ++i

      ELSE
         EXIT
      ENDIF
   NEXT

RETURN

/* -------------------------------------------------------------------------
hc_selectColor( cAttr, lMode ) --> cColor
------------------------------------------------------------------------- */
STATIC FUNCTION hc_selectColor( cAttr, lMode )

   LOCAL cColor

   IF lMode == .T.
      cColor := "EAEAEA/323232"  // Kolor dla pozostałych plików
   ELSEIF cAttr $ "DH,AH"
      cColor := "EAEAEA/72A0E5"
   ELSE
      cColor := "EAEAEA/B30000" // Kolor na zaznaczonych plikach
   ENDIF

RETURN cColor

/* -------------------------------------------------------------------------
hc_findLongestName( aSelectedPanel ) --> nLongestName
------------------------------------------------------------------------- */
STATIC FUNCTION hc_findLongestName( aSelectedPanel )

   LOCAL i
   LOCAL nCurrentNameLength
   LOCAL nLongestName := 0

   FOR i := 1 TO aSelectedPanel[ _filesCount ]

      nCurrentNameLength := Len( aSelectedPanel[ _directory ][ i ][ F_NAME ] )

      IF nCurrentNameLength > nLongestName
         nLongestName := nCurrentNameLength
      ENDIF

   NEXT

RETURN nLongestName

/* -------------------------------------------------------------------------
hc_findLongestSize( aSelectedPanel ) --> nLongestSize
------------------------------------------------------------------------- */
STATIC FUNCTION hc_findLongestSize( aSelectedPanel )

   LOCAL i
   LOCAL nCurrentSizeLength
   LOCAL nLongestSize := 0

   FOR i := 1 TO aSelectedPanel[ _filesCount ]

      nCurrentSizeLength := LenNum( aSelectedPanel[ _directory ][ i ][ F_SIZE ] )

      IF nCurrentSizeLength > nLongestSize
         nLongestSize := nCurrentSizeLength
      ENDIF

   NEXT

RETURN nLongestSize

/* -------------------------------------------------------------------------
hc_findLongestSize( aSelectedPanel ) --> nLongestAttr
------------------------------------------------------------------------- */
STATIC FUNCTION hc_findLongestAttr( aSelectedPanel )

   LOCAL i
   LOCAL currentAttrLength
   LOCAL nLongestAttr := 0

   FOR i := 1 TO aSelectedPanel[ _filesCount ]

      currentAttrLength := Len( aSelectedPanel[ _directory ][ i ][ F_ATTR ] )

      IF currentAttrLength > nLongestAttr
         nLongestAttr := currentAttrLength
      ENDIF
   NEXT

RETURN nLongestAttr

/* -------------------------------------------------------------------------
hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr, cName, cSize, dDate, cTime, cAttr ) --> cFormattedLine
------------------------------------------------------------------------- */
STATIC FUNCTION hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr, cName, cSize, dDate, cTime, cAttr )

   LOCAL nLengthSize
   LOCAL nLengthAttr
   LOCAL nParentDir := 4
   LOCAL nBorder    := 2
   LOCAL cFormattedLine
   LOCAL cPadLAttr, cPadLSize, cSizeAttrDateTime
   LOCAL cLBracket := "["
   LOCAL cRBracket := "]"
   LOCAL cPadLSizeAttrDateTime
   LOCAL nAvailableWidthForName, cCutName, cPadRName

   LOCAL cDate := DToC( dDate )

   // Ustal długości i wyrównanie na podstawie widocznych elementów panelu
   IIF( aSelectedPanel[ _isAttrVisible ], nLengthAttr := nLongestAttr, nLengthAttr := 0 )
   IIF( aSelectedPanel[ _isSizeVisible ], nLengthSize := nLongestSize, nLengthSize := 0 )

   // Wyrównanie
   cPadLAttr := PadL( cAttr, nLengthAttr )
   cPadLSize := PadL( cSize, nLengthSize )

   IF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isDateVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isDateVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cDate
   ELSEIF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cTime
   ELSEIF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isAttrVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr
   ELSEIF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isDateVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _isSizeVisible  ] .AND. aSelectedPanel[ _isDateVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cDate
   ELSEIF aSelectedPanel[ _isSizeVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cTime
   ELSEIF aSelectedPanel[ _isSizeVisible ]
      cSizeAttrDateTime := cPadLSize
   ELSEIF aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isDateVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isDateVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cDate
   ELSEIF aSelectedPanel[ _isAttrVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cTime
   ELSEIF aSelectedPanel[ _isAttrVisible ]
      cSizeAttrDateTime := cPadLAttr
   ELSEIF aSelectedPanel[ _isDateVisible ] .AND. aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cDate + " " + cTime
   ELSEIF aSelectedPanel[ _isDateVisible ]
      cSizeAttrDateTime := cDate
   ELSEIF aSelectedPanel[ _isTimeVisible ]
      cSizeAttrDateTime := cTime
   ELSE
      cSizeAttrDateTime := " "
   ENDIF

   IF cName == ".."
      cPadLSizeAttrDateTime := PadL( cSizeAttrDateTime, aSelectedPanel[ _maxCol ] - nBorder - nParentDir )
      cFormattedLine := cLBracket + cName + cRBracket + cPadLSizeAttrDateTime
   ELSE
      cPadLSizeAttrDateTime := PadL( cSizeAttrDateTime, aSelectedPanel[ _maxCol ] - nBorder - nLongestName )
      nAvailableWidthForName := aSelectedPanel[ _maxCol ] - nBorder - Len( cPadLSizeAttrDateTime )
      cCutName := Left( cName, nAvailableWidthForName )
      cPadRName := PadR( cCutName, nAvailableWidthForName )
      cFormattedLine := cPadRName + cPadLSizeAttrDateTime
   ENDIF

RETURN cFormattedLine

/* *************************************************************************
Harbour C Code
************************************************************************* */
#pragma BEGINDUMP

#include <hbapi.h>

typedef enum _bool bool;
enum _bool
{
   F = 0,
   T = ( ! 0 )
};

HB_FUNC( HC_TOGGLEPANELS )
{
   bool visiblePanels = hb_parl( 1 );
   visiblePanels = !visiblePanels;
   hb_retl( visiblePanels );
}

#pragma ENDDUMP

