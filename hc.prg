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

/*
 * Prefixes used in the code:
 *
 * `a` - aArray (arrays)
 * `b` - bBlock (code blocks)
 * `c` - cCharacter (strings)
 * `d` - dDate (dates)
 * `l` - lLogical (booleans)
 * `m` - mMemo (memo fields)
 * `n` - nNumeric (numbers)
 * `o` - oObject (objects)
 * `p` - pPointer (pointers)
 * `s` - sSymbol (symbols)
 * `u` - uNIL (undefined or NIL values)
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
#define _nCol                  1
#define _nRow                  2
#define _nMaxCol               3
#define _nMaxRow               4
#define _cCurrentDir           5
#define _aDirectory            6
#define _nItemCount           7
#define _nRowBar               8
#define _nRowNo                9
#define _cCmdLine             10
#define _cCmdOutput           11
#define _nCmdCol              12
#define _nCmdColNo            13
#define _lIsFirstDirectory    14
#define _lIsSizeVisible       15
#define _lIsAttrVisible       16
#define _lIsDateVisible       17
#define _lIsTimeVisible       18

#define _nElements            18
// ---
#define F_MODE                 6
// ---

PROCEDURE Main()

   LOCAL lQuit := .F.

   LOCAL pApp
   LOCAL pEvent

   LOCAL aLeftPanel
   LOCAL aRightPanel
   LOCAL aActivePanel

   LOCAL lWaitMode := .T.
   LOCAL nTimeBeg
   LOCAL nTimeEnd

   LOCAL lVisiblePanels := .T.

   pApp := sdl_CreateWindow( 830, 450, "Harbour Commander", "F1F1F1" )

   sdl_LoadFont( pApp, "./font/9x18.pcf.gz", 18 )

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   aLeftPanel  := hc_init()
   aRightPanel := hc_init()

   Set( _SET_DATEFORMAT, "dd-mm-yyyy" )

   aLeftPanel  := hc_fetchList( aLeftPanel, hb_cwd() )
   aRightPanel := hc_fetchList( aRightPanel, hb_cwd() )

   aActivePanel := aLeftPanel

   nTimeBeg := C_time()

   DO WHILE( !lQuit )

      nTimeEnd := C_time()

      IF( lWaitMode .OR. C_difftime( nTimeEnd, nTimeBeg ) >= 10 )

         sdl_setCursorVisible( pApp, .T. )
         pEvent := sdl_WaitEvent()
         IF( pEvent != NIL )
            nTimeBeg := C_time()
            lWaitMode := .F.
         ENDIF

      ELSE

         DO WHILE( ( pEvent := sdl_PollEvent() ) != NIL )

            nTimeBeg := C_time()

            SWITCH( sdl_EventType( pEvent ) )

               CASE SDL_QUIT

                  lQuit := .T.
                  EXIT

               CASE SDL_WINDOWEVENT

                  IF( sdl_EventWindowEvent( pEvent ) == SDL_WINDOWEVENT_CLOSE )
                     lQuit := .T.
                  ENDIF
                  EXIT

               CASE SDL_KEYDOWN

                  SWITCH( sdl_EventKeyKeysymSym( pEvent ) )

                     CASE SDLK_ESCAPE
                        lQuit := .T.
                        EXIT

                     CASE SDLK_TAB

                        IF( lVisiblePanels )
                           IF( aActivePanel == aLeftPanel )
                              aActivePanel := aRightPanel
                              aActivePanel[ _cCmdLine ] := aLeftPanel[ _cCmdLine ]
                              aActivePanel[ _nCmdCol  ] := aLeftPanel[ _nCmdCol ]
                              aLeftPanel[   _cCmdLine ] := ""
                              aLeftPanel[   _nCmdCol  ] := 0
                           ELSE
                              aActivePanel := aLeftPanel
                              aActivePanel[ _cCmdLine ] := aRightPanel[ _cCmdLine ]
                              aActivePanel[ _nCmdCol  ] := aRightPanel[ _nCmdCol ]
                              aRightPanel[  _cCmdLine ] := ""
                              aRightPanel[  _nCmdCol  ] := 0
                           ENDIF
                        ENDIF
                        EXIT

                     CASE SDLK_DOWN

                        IF( aActivePanel[ _nRowBar ] < aActivePanel[ _nMaxRow ] - 1 .AND. aActivePanel[ _nRowBar ] <= Len( aActivePanel[ _aDirectory ] ) - 1 )
                           ++aActivePanel[ _nRowBar ]
                        ELSE
                           IF( aActivePanel[ _nRowNo ] + aActivePanel[ _nRowBar ] <= Len( aActivePanel[ _aDirectory ] ) - 1 )
                              ++aActivePanel[ _nRowNo ]
                           ENDIF
                        ENDIF
                        EXIT

                     OTHERWISE

                        IF( sdl_EventKeyKeysymSym( pEvent ) == SDLK_o .AND. hb_BitAnd( sdl_GetModState(), KMOD_LCTRL ) != 0 )
                           lVisiblePanels := hc_togglePanels( lVisiblePanels )
                        ENDIF
                        EXIT

                  ENDSWITCH

               OTHERWISE

            ENDSWITCH

         ENDDO

         IF( C_difftime( nTimeEnd, nTimeBeg ) >= 10 )
            lWaitMode := .T.
         ENDIF

      ENDIF

      sdl_BeginDraw( pApp )

         IF( lVisiblePanels )
            aLeftPanel := hc_resize( aLeftPanel, 0, 0, sdl_maxCol( pApp ) / 2, sdl_maxRow( pApp ) - 1 )
            aRightPanel := hc_resize( aRightPanel, sdl_maxCol( pApp ) / 2, 0, sdl_maxCol( pApp ) / 2, sdl_maxRow( pApp ) - 1 )

            hc_drawPanel( pApp, aActivePanel, aLeftPanel )
            hc_drawPanel( pApp, aActivePanel, aRightPanel )
            sdl_setBackground( pApp, "F1F1F1" )
         ELSE
            sdl_setBackground( pApp, "000000" )
         ENDIF

      sdl_EndDraw( pApp )

   ENDDO

RETURN

/* -------------------------------------------------------------------------
hc_init() --> aPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_init()

   LOCAL aPanel

   aPanel := Array( _nElements )

   aPanel[ _nCol              ] :=  0
   aPanel[ _nRow              ] :=  0
   aPanel[ _nMaxCol           ] :=  0
   aPanel[ _nMaxRow           ] :=  0
   aPanel[ _cCurrentDir       ] :=  ""
   aPanel[ _aDirectory        ] :=  {}
   aPanel[ _nItemCount       ] :=  0
   aPanel[ _nRowBar           ] :=  1
   aPanel[ _nRowNo            ] :=  0
   aPanel[ _cCmdLine          ] :=  ""
   aPanel[ _cCmdOutput        ] :=  ""
   aPanel[ _nCmdCol           ] :=  0
   aPanel[ _nCmdColNo         ] :=  0
   aPanel[ _lIsFirstDirectory ] := .T.
   aPanel[ _lIsSizeVisible    ] := .T.
   aPanel[ _lIsAttrVisible    ] := .T.
   aPanel[ _lIsDateVisible    ] := .T.
   aPanel[ _lIsTimeVisible    ] := .T.

   RETURN aPanel

/* -------------------------------------------------------------------------
hc_fetchList( aSelectedPanel, cDir ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_fetchList( aSelectedPanel, cDir )

   // Ustawienie bieżącego katalogu
   aSelectedPanel[ _cCurrentDir ] := hb_defaultValue( cDir, hb_cwd() )
   aSelectedPanel[ _aDirectory ] := Directory( aSelectedPanel[ _cCurrentDir ], "HSD" )

   // Usunięcie wpisu bieżącego katalogu "." z listy
   hb_ADel( aSelectedPanel[ _aDirectory ], AScan( aSelectedPanel[ _aDirectory ], { | x | x[ F_NAME ] == "." } ), .T. )

   // Dodanie wartości logicznej `.T.` na końcu każdego wpisu w tablicy
   AEval( aSelectedPanel[ _aDirectory ], { | x | AAdd( x, .T. ) } )
   aSelectedPanel[ _nItemCount ] := Len( aSelectedPanel[ _aDirectory ] )

   // 1. Katalog ".." zawsze na początku
   ASort( aSelectedPanel[ _aDirectory ],,, { | x | x[ F_NAME ] == ".." } )

   // 2. Katalog "." zaraz po ".." (nie jest drukowany w hc_drawPanel )
   ASORT( aSelectedPanel[ _aDirectory ],,, { | x, y | ( x[ F_NAME ] == "." ) .AND. ( y[ F_NAME ] != ".." ) } )

   // 3. Zwykłe katalogi przed ukrytymi katalogami
   ASort( aSelectedPanel[ _aDirectory ],,, { | x, y | "D" $ x[ F_ATTR ] .AND. !( "H" $ x[ F_ATTR ] ) .AND. ( "H" $ y[ F_ATTR ] ) } )

   // 4. Katalogi przed plikami (bez względu na ukrycie)
   ASort( aSelectedPanel[ _aDirectory ],,, { | x, y | "D" $ x[ F_ATTR ] .AND. !( "D" $ y[ F_ATTR ] ) } )

RETURN aSelectedPanel

/* -------------------------------------------------------------------------
hc_resize( aSelectedPanel, nCol, nRow, nMaxCol, nMaxRow ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_resize( aSelectedPanel, nCol, nRow, nMaxCol, nMaxRow )

   aSelectedPanel[ _nCol    ] := nCol
   aSelectedPanel[ _nRow    ] := nRow
   aSelectedPanel[ _nMaxCol ] := nMaxCol
   aSelectedPanel[ _nMaxRow ] := nMaxRow

RETURN aSelectedPanel

/* -------------------------------------------------------------------------
hc_drawPanel( pApp, aActivePanel, aSelectedPanel ) --> NIL
------------------------------------------------------------------------- */
STATIC PROCEDURE hc_drawPanel( pApp, aActivePanel, aSelectedPanel )

   LOCAL i := 1
   LOCAL nRow
   LOCAL nLongestName := 4
   LOCAL nLongestSize
   LOCAL nLongestAttr
   LOCAL cPaddedString
   LOCAL cPaddedResult
   LOCAL cSelectedColor

   IF( aActivePanel == aSelectedPanel )
      sdl_drawBox( pApp, aSelectedPanel[ _nCol ], aSelectedPanel[ _nRow ], aSelectedPanel[ _nMaxCol ], aSelectedPanel[ _nMaxRow ], BOX_DOUBLE, "F1F1F1/323232" )
   ELSE
      sdl_drawBox( pApp, aSelectedPanel[ _nCol ], aSelectedPanel[ _nRow ], aSelectedPanel[ _nMaxCol ], aSelectedPanel[ _nMaxRow ], BOX_SINGLE, "F1F1F1/323232" )
   ENDIF

   nLongestName := MAX( nLongestName, hc_findLongestName( aSelectedPanel ) )
   nLongestSize := hc_findLongestSize( aSelectedPanel )
   nLongestAttr := hc_findLongestAttr( aSelectedPanel )

   i += aSelectedPanel[ _nRowNo ]
   FOR nRow := aSelectedPanel[ _nRow ] + 1 TO aSelectedPanel[ _nMaxRow ] - 1

      IF( i <= aSelectedPanel[ _nItemCount ] )

         cPaddedString := hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr,;
            aSelectedPanel[ _aDirectory ][ i ][ F_NAME ],;
            aSelectedPanel[ _aDirectory ][ i ][ F_SIZE ],;
            aSelectedPanel[ _aDirectory ][ i ][ F_DATE ],;
            aSelectedPanel[ _aDirectory ][ i ][ F_TIME ],;
            aSelectedPanel[ _aDirectory ][ i ][ F_ATTR ] )

         cPaddedResult := PadR( cPaddedString, aSelectedPanel[ _nMaxCol ] - 2 )

         IF( aActivePanel == aSelectedPanel .AND. i == aSelectedPanel[ _nRowBar ] + aSelectedPanel[ _nRowNo ] )

            IF( !aSelectedPanel[ _aDirectory ][ i ][ F_MODE ] )
               cSelectedColor := "323232/FF4D4D"
            ELSE
               cSelectedColor := "323232/00FF00"
            ENDIF

         ELSE
            cSelectedColor := hc_selectColor( aSelectedPanel[ _aDirectory ][ i ][ F_ATTR ], aSelectedPanel[ _aDirectory ][ i ][ F_MODE ] )
         ENDIF

         sdl_drawFont( pApp, aSelectedPanel[ _nCol ] + 1, nRow, cPaddedResult, cSelectedColor )

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

   IF( lMode == .T. )
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

   FOR i := 1 TO aSelectedPanel[ _nItemCount ]

      nCurrentNameLength := Len( aSelectedPanel[ _aDirectory ][ i ][ F_NAME ] )

      IF( nCurrentNameLength > nLongestName )
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

   FOR i := 1 TO aSelectedPanel[ _nItemCount ]

      nCurrentSizeLength := LenNum( aSelectedPanel[ _aDirectory ][ i ][ F_SIZE ] )

      IF( nCurrentSizeLength > nLongestSize )
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

   FOR i := 1 TO aSelectedPanel[ _nItemCount ]

      currentAttrLength := Len( aSelectedPanel[ _aDirectory ][ i ][ F_ATTR ] )

      IF( currentAttrLength > nLongestAttr )
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
   IIF( aSelectedPanel[ _lIsAttrVisible ], nLengthAttr := nLongestAttr, nLengthAttr := 0 )
   IIF( aSelectedPanel[ _lIsSizeVisible ], nLengthSize := nLongestSize, nLengthSize := 0 )

   // Wyrównanie
   cPadLAttr := PadL( cAttr, nLengthAttr )
   cPadLSize := PadL( cSize, nLengthSize )

   IF( aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsDateVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ] )
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsDateVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cDate
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr + " " + cTime
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsAttrVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cPadLAttr
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsDateVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _lIsSizeVisible  ] .AND. aSelectedPanel[ _lIsDateVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cDate
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cPadLSize + " " + cTime
   ELSEIF aSelectedPanel[ _lIsSizeVisible ]
      cSizeAttrDateTime := cPadLSize
   ELSEIF aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsDateVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cDate + " " + cTime
   ELSEIF aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsDateVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cDate
   ELSEIF aSelectedPanel[ _lIsAttrVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cPadLAttr + " " + cTime
   ELSEIF aSelectedPanel[ _lIsAttrVisible ]
      cSizeAttrDateTime := cPadLAttr
   ELSEIF aSelectedPanel[ _lIsDateVisible ] .AND. aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cDate + " " + cTime
   ELSEIF aSelectedPanel[ _lIsDateVisible ]
      cSizeAttrDateTime := cDate
   ELSEIF aSelectedPanel[ _lIsTimeVisible ]
      cSizeAttrDateTime := cTime
   ELSE
      cSizeAttrDateTime := " "
   ENDIF

   IF( cName == ".." )
      cPadLSizeAttrDateTime := PadL( cSizeAttrDateTime, aSelectedPanel[ _nMaxCol ] - nBorder - nParentDir )
      cFormattedLine := cLBracket + cName + cRBracket + cPadLSizeAttrDateTime
   ELSE
      cPadLSizeAttrDateTime := PadL( cSizeAttrDateTime, aSelectedPanel[ _nMaxCol ] - nBorder - nLongestName )
      nAvailableWidthForName := aSelectedPanel[ _nMaxCol ] - nBorder - Len( cPadLSizeAttrDateTime )
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

