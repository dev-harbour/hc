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

// Keeping it tidy
#pragma -w3
#pragma -es2

// Optimizations
#pragma -km+
#pragma -ko+

#include "hbsdl.ch"

// Harbour
#include "directry.ch"

// ---
#define BLACK            "0C0C0C"
#define BLUE             "0037DA"
#define GREEN            "13A10E"
#define CYAN             "3A96DD"
#define RED              "C50F1F"
#define MAGENTA          "881798"
#define BROWN            "C19C00"
#define LIGHT_GRAY       "CCCCCC"
#define GRAY             "767676"
#define LIGHT_BLUE       "3B78FF"
#define LIGHT_GREEN      "16C60C"
#define LIGHT_CYAN       "61D6D6"
#define LIGHT_RED        "E74856"
#define LIGHT_MAGENTA    "B4009E"
#define YELLOW           "F9F1A5"
#define WHITE            "F2F2F2"
// ---
#define ATTR_REGULAR          "A"
#define ATTR_EXECUTABLE       "E"
#define ATTR_DIRECTORY        "D"
#define ATTR_HIDDEN           "H"
// ---
#define _nCol                  1
#define _nRow                  2
#define _nMaxCol               3
#define _nMaxRow               4
#define _cCurrentDir           5
#define _aDirList              6
#define _nItemCount            7
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

   LOCAL nMaxCol
   LOCAL nMaxRow

   LOCAL lVisiblePanels := .T.

   LOCAL nIndex
   LOCAL cCommandLine

   LOCAL cKeyChar

   LOCAL aCmdOutput, i

   pApp := sdl_CreateWindow( 801, 450, "Harbour Commander", WHITE )

   sdl_LoadFont( pApp, "./font/9x18.pcf.gz", 18 )

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   aLeftPanel  := hc_init()
   aRightPanel := hc_init()

   Set( _SET_DATEFORMAT, "dd-mm-yyyy" )

   nMaxCol := sdl_maxCol( pApp )
   nMaxRow := sdl_maxRow( pApp )

   aLeftPanel  := hc_fetchList( aLeftPanel, hb_cwd() )
   aRightPanel := hc_fetchList( aRightPanel, hb_cwd() )

   aActivePanel := aLeftPanel

   // Zwraca bieżący czas w sekundach jako liczbę całkowitą
   nTimeBeg := C_time()

   DO WHILE( !lQuit )

      nTimeEnd := C_time()

      // Oblicza różnicę w sekundach między nTimeEnd a nTimeBeg
      IF( lWaitMode .OR. C_difftime( nTimeEnd, nTimeBeg ) >= 10 )

         sdl_setCursorVisible( pApp, .T. )
         pEvent := sdl_WaitEvent()
         IF( pEvent != NIL )
            // Aktualizacja czasu przy nowym zdarzeniu
            nTimeBeg := C_time()
            lWaitMode := .F.
         ENDIF

      ELSE

         DO WHILE( ( pEvent := sdl_PollEvent() ) != NIL )

            // Aktualizacja czasu przy każdym zdarzeniu
            nTimeBeg := C_time()

            SWITCH( sdl_EventType( pEvent ) )

               CASE SDL_QUIT

                  lQuit := .T.
                  EXIT

               CASE SDL_WINDOWEVENT

                  SWITCH( sdl_EventWindowEvent( pEvent ) )

                     CASE SDL_WINDOWEVENT_CLOSE
                        lQuit := .T.
                        EXIT

                     CASE SDL_WINDOWEVENT_RESIZED
                        nMaxCol := sdl_maxCol( pApp )
                        nMaxRow := sdl_maxRow( pApp )
                        EXIT

                  ENDSWITCH
                  EXIT

               CASE SDL_KEYDOWN

                  SWITCH( sdl_EventKeyKeysymSym( pEvent ) )

                     CASE SDLK_ESCAPE
                        lQuit := .T.
                        EXIT

                     CASE SDLK_RETURN

                        IF( aActivePanel[ _cCmdLine ] == "" )

                           IF( lVisiblePanels )
                              // Określ indeks w panelu i sprawdź, czy wybrany element to katalog
                              nIndex := aActivePanel[ _nRowBar ] + aActivePanel[ _nRowNo ]
                              IF( At( ATTR_DIRECTORY, aActivePanel[ _aDirList ][ nIndex ][ F_ATTR ] ) == 0 )
                                 // Przygotowanie pełnej ścieżki dla wybranego elementu
                                 cCommandLine := '"' + aActivePanel[ _cCurrentDir ] + aActivePanel[ _aDirList ][ nIndex ][ F_NAME ] + '"'
                                 // Sprawdzenie, czy element jest wykonywalnym plikiem
                                 IF( hc_isExecutable( cCommandLine ) )
                                    hc_runApp( cCommandLine )
                                 ELSE
                                    hc_openFile( cCommandLine )
                                 ENDIF

                              ELSE
                                 // Zmiana katalogu, jeśli wybrano katalog
                                 aActivePanel := hc_changeDir( aActivePanel )
                              ENDIF
                           ENDIF

                        ELSE

                           // Zmiana katalogu w przypadku błędu
                           IF( hc_chDir( aActivePanel[ _cCurrentDir ] ) == .F. )
                              C_perror( e"\nError changing directory\n" )
                           ENDIF

                           // Uruchomienie polecenia i przechwycenie jego wyniku
                           IF( C_system( aActivePanel[ _cCmdLine ] ) == - 1 )
                              C_perror( e"\nError executing command\n" )
                           ELSE
                              aActivePanel[ _cCmdOutput ] := hc_executeAndCapture( aActivePanel[ _cCmdLine ] )
                           ENDIF

                           aActivePanel[ _cCmdLine ] := ""
                           aActivePanel[ _nCmdCol ]  := 0

                           hc_refreshPanels( pApp, aActivePanel, aLeftPanel, aRightPanel, lVisiblePanels )

                        ENDIF
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

                     CASE SDLK_LEFT

                        IF( aActivePanel[ _nCmdCol ] > 0 )
                           aActivePanel[ _nCmdCol ]--
                        ELSE
                           IF( aActivePanel[ _nCmdColNo ] >= 1 )
                              aActivePanel[ _nCmdColNo ]--
                           ENDIF
                        ENDIF
                        EXIT

                     CASE SDLK_RIGHT

                        IF( aActivePanel[ _nCmdCol ] < nMaxCol - Len( aActivePanel[ _cCurrentDir ] ) .AND. aActivePanel[ _nCmdCol ] < Len( aActivePanel[ _cCmdLine ] ) )
                           aActivePanel[ _nCmdCol ]++
                        ELSE
                           IF( aActivePanel[ _nCmdColNo ] + aActivePanel[ _nCmdCol ] < Len( aActivePanel[ _cCmdLine ] ) )
                              aActivePanel[ _nCmdColNo ]++
                           ENDIF
                        ENDIF
                        EXIT

                     CASE SDLK_HOME

                        aActivePanel[ _nCmdCol ] := 0
                        EXIT

                     CASE SDLK_END

                        aActivePanel[ _nCmdCol ] := Len( aActivePanel[ _cCmdLine ] )
                        EXIT

                     CASE SDLK_DELETE

                        IF( aActivePanel[ _nCmdCol ] >= 0 )
                           aActivePanel[ _cCmdLine ] := Stuff( aActivePanel[ _cCmdLine ], aActivePanel[ _nCmdCol ] + 1, 1, "" )
                        ENDIF
                        EXIT

                     CASE SDLK_BACKSPACE

                        IF( aActivePanel[ _nCmdCol ] > 0 )
                           aActivePanel[ _cCmdLine ] := Stuff( aActivePanel[ _cCmdLine ], aActivePanel[ _nCmdCol ], 1, "" )
                           aActivePanel[ _nCmdCol ]--
                        ENDIF
                        EXIT

                     CASE SDLK_INSERT

                        nIndex := aActivePanel[ _nRowBar ] + aActivePanel[ _nRowNo ]
                        IF( aActivePanel[ _aDirList ][ nIndex ][ F_NAME ] != ".." )

                           IF( aActivePanel[ _aDirList ][ nIndex ][ F_MODE ] )
                              aActivePanel[ _aDirList ][ nIndex ][ F_MODE ] := .F.
                           ELSE
                              aActivePanel[ _aDirList ][ nIndex ][ F_MODE ] := .T.
                           ENDIF

                           IF( aActivePanel[ _nRowBar ] < aActivePanel[ _nMaxRow ] - 1 .AND. aActivePanel[ _nRowBar ] <= aActivePanel[ _nItemCount ] - 1 )
                              ++aActivePanel[ _nRowBar ]
                           ELSE
                              IF( aActivePanel[ _nRowNo ] + aActivePanel[ _nRowBar ] <= aActivePanel[ _nItemCount ] - 1 )
                                 ++aActivePanel[ _nRowNo ]
                              ENDIF
                           ENDIF

                        ENDIF
                        EXIT

                     CASE SDLK_DOWN

                        IF( lVisiblePanels )
                           IF( aActivePanel[ _nRowBar ] < aActivePanel[ _nMaxRow ] - 2 .AND. aActivePanel[ _nRowBar ] <= aActivePanel[ _nItemCount ] - 1 )
                              ++aActivePanel[ _nRowBar ]
                           ELSE
                              IF( aActivePanel[ _nRowNo ] + aActivePanel[ _nRowBar ] <= aActivePanel[ _nItemCount ] - 1 )
                                 ++aActivePanel[ _nRowNo ]
                              ENDIF
                           ENDIF
                        ENDIF
                        EXIT

                     CASE SDLK_UP

                        IF( lVisiblePanels )
                           IF( aActivePanel[ _nRowBar ] > 1 )
                              --aActivePanel[ _nRowBar ]
                           ELSE
                              IF( aActivePanel[ _nRowNo ] >= 1 )
                                 --aActivePanel[ _nRowNo ]
                              ENDIF
                           ENDIF
                        ENDIF
                        EXIT

                     CASE SDLK_PAGEDOWN

                        IF( lVisiblePanels )
                           IF( aActivePanel[ _nRowBar ] >= nMaxRow - 3 )
                              IF( aActivePanel[ _nRowNo ] + nMaxRow  <= aActivePanel[ _nItemCount ] )
                                 aActivePanel[ _nRowNo ] += nMaxRow
                              ENDIF
                           ENDIF
                           aActivePanel[ _nRowBar ] := Min( nMaxRow - 3, aActivePanel[ _nItemCount ] - aActivePanel[ _nRowNo ] )
                        ENDIF
                        EXIT

                     CASE SDLK_PAGEUP

                        IF( lVisiblePanels )
                           IF( aActivePanel[ _nRowBar ] <= 1 )
                              IF( aActivePanel[ _nRowNo ] - nMaxRow >= 0 )
                                 aActivePanel[ _nRowNo ] -= nMaxRow
                              ENDIF
                           ENDIF
                           aActivePanel[ _nRowBar ] := 1
                        ENDIF
                        EXIT

                     OTHERWISE

                        IF( sdl_EventKeyKeysymSym( pEvent ) == SDLK_o .AND. hb_BitAnd( sdl_GetModState(), KMOD_LCTRL ) != 0 )
                           lVisiblePanels := hc_isTogglePanels( lVisiblePanels )
                        ENDIF
                        EXIT

                  ENDSWITCH

               CASE SDL_MOUSEWHEEL
                  EXIT

               CASE SDL_MOUSEMOTION
                  EXIT

               CASE SDL_MOUSEBUTTONDOWN
                  EXIT

               OTHERWISE

                  cKeyChar := sdl_keyChar( pEvent )
                  IF( !cKeyChar == "" )

                     aActivePanel[ _cCmdLine ] := Stuff( aActivePanel[ _cCmdLine ], aActivePanel[ _nCmdCol ] + aActivePanel[ _nCmdColNo ] + 1, 0, sdl_keyChar( pEvent ) )
                     IF( aActivePanel[ _nCmdCol ] < nMaxCol - aActivePanel[ _nItemCount ] )
                        aActivePanel[ _nCmdCol ]++
                     ELSE
                        aActivePanel[ _nCmdColNo ]++
                     ENDIF

                  ENDIF

            ENDSWITCH

         ENDDO

         /* Jeśli różnica czasu przekroczy 10 sekund, przełącza na tryb oczekiwania */
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
            sdl_setBackground( pApp, WHITE )
         ELSE
            sdl_setBackground( pApp, BLACK )

            aCmdOutput := hb_ATokens( aActivePanel[ _cCmdOutput ], .T. )
            FOR i := 1 TO Len( aCmdOutput )
               sdl_drawFont( pApp, 1, i, aCmdOutput[ i ] , BLACK + "/" + RED )
            NEXT

         ENDIF

         hc_drawCmdLine( pApp, aActivePanel, lVisiblePanels )

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
   aPanel[ _aDirList          ] :=  {}
   aPanel[ _nItemCount        ] :=  0
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
hc_fetchList( aSelectedPanel, cCurrentDir ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_fetchList( aSelectedPanel, cCurrentDir )

   // Ustawienie bieżącego katalogu
   aSelectedPanel[ _cCurrentDir ] := hb_defaultValue( cCurrentDir, hb_cwd() )
   aSelectedPanel[ _aDirList ]    := Directory( aSelectedPanel[ _cCurrentDir ], "HSD" )

   // Usunięcie wpisu bieżącego katalogu "." z listy
   hb_ADel( aSelectedPanel[ _aDirList ], AScan( aSelectedPanel[ _aDirList ], { | x | x[ F_NAME ] == "." } ), .T. )

   // Dodanie wartości logicznej `.T.` na końcu każdego wpisu w tablicy
   AEval( aSelectedPanel[ _aDirList ], { | x | AAdd( x, .T. ) } )

   aSelectedPanel[ _nItemCount ] := Len( aSelectedPanel[ _aDirList ] )

   // 1. Katalog ".." zawsze na początku
   ASort( aSelectedPanel[ _aDirList ],,, { | x | x[ F_NAME ] == ".." } )

   // 2. Katalog "." zaraz po ".." (nie jest drukowany w hc_drawPanel )
   ASORT( aSelectedPanel[ _aDirList ],,, { | x, y | ( x[ F_NAME ] == "." ) .AND. ( y[ F_NAME ] != ".." ) } )

   // 3. Zwykłe katalogi przed ukrytymi katalogami
   ASort( aSelectedPanel[ _aDirList ],,, { | x, y | ATTR_DIRECTORY $ x[ F_ATTR ] .AND. !( ATTR_HIDDEN $ x[ F_ATTR ] ) .AND. ( ATTR_HIDDEN $ y[ F_ATTR ] ) } )

   // 4. Katalogi przed plikami (bez względu na ukrycie)
   ASort( aSelectedPanel[ _aDirList ],,, { | x, y | ATTR_DIRECTORY $ x[ F_ATTR ] .AND. !( ATTR_DIRECTORY $ y[ F_ATTR ] ) } )

RETURN aSelectedPanel

/* -------------------------------------------------------------------------
hc_refreshPanels( pApp, aSelectedPanel, aLeftPanel, aRightPanel, lVisiblePanels ) --> NIL
------------------------------------------------------------------------- */
STATIC PROCEDURE hc_refreshPanels( pApp, aSelectedPanel, aLeftPanel, aRightPanel, lVisiblePanels )

   // Sprawdź, czy oba panele są w tym samym katalogu
   IF( aLeftPanel[ _cCurrentDir ] == aRightPanel[ _cCurrentDir ] )

      aLeftPanel := hc_fetchList( aLeftPanel, aLeftPanel[ _cCurrentDir ] )
      aRightPanel := hc_fetchList( aRightPanel, aRightPanel[ _cCurrentDir ] )

      IF( lVisiblePanels )
         sdl_BeginDraw( pApp )
            hc_drawPanel( pApp, aSelectedPanel, aLeftPanel )
            hc_drawPanel( pApp, aSelectedPanel, aRightPanel )
         sdl_EndDraw( pApp )
      ENDIF

   ELSE

      // Odśwież tylko wybrany panel
      aSelectedPanel := hc_fetchList( aSelectedPanel, aSelectedPanel[ _cCurrentDir ] )

      IF( lVisiblePanels )
         sdl_BeginDraw( pApp )
            hc_drawPanel( pApp, aSelectedPanel, aSelectedPanel )
         sdl_EndDraw( pApp )
      ENDIF

   ENDIF

RETURN

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

   // Rysowanie ramki dla aktywnego lub nieaktywnego panelu
   IF( aActivePanel == aSelectedPanel )
      sdl_drawBox( pApp, aSelectedPanel[ _nCol ], aSelectedPanel[ _nRow ], aSelectedPanel[ _nMaxCol ], aSelectedPanel[ _nMaxRow ], BOX_DOUBLE, WHITE + "/" + BLACK )
   ELSE
      sdl_drawBox( pApp, aSelectedPanel[ _nCol ], aSelectedPanel[ _nRow ], aSelectedPanel[ _nMaxCol ], aSelectedPanel[ _nMaxRow ], BOX_SINGLE, WHITE + "/" + BLACK )
   ENDIF

   // Ustal najdłuższą nazwę, rozmiar i atrybut, by wyśrodkować elementy
   nLongestName := MAX( nLongestName, hc_findLongestName( aSelectedPanel ) )
   nLongestSize := hc_findLongestSize( aSelectedPanel )
   nLongestAttr := hc_findLongestAttr( aSelectedPanel )

   // Przesuń indeks o bieżący numer wiersza
   i += aSelectedPanel[ _nRowNo ]
   FOR nRow := aSelectedPanel[ _nRow ] + 1 TO aSelectedPanel[ _nMaxRow ] - 2

      // Sprawdzenie, czy indeks jest w granicach liczby elementów
      IF( i <= aSelectedPanel[ _nItemCount ] )

         // Utwórz wyśrodkowany ciąg z informacjami o pliku lub katalogu
         cPaddedString := hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr,;
            aSelectedPanel[ _aDirList ][ i ][ F_NAME ],;
            aSelectedPanel[ _aDirList ][ i ][ F_SIZE ],;
            aSelectedPanel[ _aDirList ][ i ][ F_DATE ],;
            aSelectedPanel[ _aDirList ][ i ][ F_TIME ],;
            aSelectedPanel[ _aDirList ][ i ][ F_ATTR ] )

         // Wyśrodkuj ciąg w kolumnie
         cPaddedResult := PadR( cPaddedString, aSelectedPanel[ _nMaxCol ] - 2 )

         // Ustaw kolor dla aktywnego elementu, zależnie od jego stanu
         IF( aActivePanel == aSelectedPanel .AND. i == aSelectedPanel[ _nRowBar ] + aSelectedPanel[ _nRowNo ] )

            IF( !aSelectedPanel[ _aDirList ][ i ][ F_MODE ] )
               cSelectedColor := BLACK + "/" + LIGHT_RED
            ELSE
               cSelectedColor := BLACK + "/" + LIGHT_GREEN
            ENDIF

         ELSE
            // Ustaw kolor na podstawie atrybutów pliku
            cSelectedColor := hc_selectColor( aSelectedPanel[ _aDirList ][ i ][ F_ATTR ], aSelectedPanel[ _aDirList ][ i ][ F_MODE ] )
         ENDIF

         // Rysowanie tekstu z określonym kolorem w danym wierszu
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

   IF( !lMode )
      RETURN WHITE + "/" + RED    // Kolor na zaznaczonych plikach
   ELSEIF( cAttr == "HD" .OR. cAttr == "HA" )
      RETURN  WHITE + "/" + CYAN  // Kolor dla ukrytych katalogów
   ELSE
      RETURN WHITE + "/" + BLACK  // Kolor dla pozostałych plików
   ENDIF

RETURN NIL

/* -------------------------------------------------------------------------
hc_findLongestName( aSelectedPanel ) --> nLongestName
------------------------------------------------------------------------- */
STATIC FUNCTION hc_findLongestName( aSelectedPanel )

   LOCAL i
   LOCAL nCurrentNameLength
   LOCAL nLongestName := 0

   FOR i := 1 TO aSelectedPanel[ _nItemCount ]

      nCurrentNameLength := Len( aSelectedPanel[ _aDirList ][ i ][ F_NAME ] )

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

      nCurrentSizeLength := LenNum( aSelectedPanel[ _aDirList ][ i ][ F_SIZE ] )

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

      currentAttrLength := Len( aSelectedPanel[ _aDirList ][ i ][ F_ATTR ] )

      IF( currentAttrLength > nLongestAttr )
         nLongestAttr := currentAttrLength
      ENDIF
   NEXT

RETURN nLongestAttr

/* -------------------------------------------------------------------------
hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr, cName, nSize, dDate, cTime, cAttr ) --> cFormattedLine
------------------------------------------------------------------------- */
STATIC FUNCTION hc_paddedString( aSelectedPanel, nLongestName, nLongestSize, nLongestAttr, cName, nSize, dDate, cTime, cAttr )

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

   // Wyrównanie atrybutów
   cPadLAttr := PadL( cAttr, nLengthAttr )

   // Ustawienie "DIR" jako rozmiaru dla katalogów, inaczej wyświetlenie faktycznego rozmiaru
   IF( ATTR_DIRECTORY $ cAttr )
      cPadLSize := PadL( "DIR", nLengthSize )
   ELSE
      cPadLSize := PadL( nSize, nLengthSize )
   ENDIF

   // Formatuj ciąg w zależności od widocznych elementów
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
   ELSEIF aSelectedPanel[ _lIsSizeVisible ] .AND. aSelectedPanel[ _lIsDateVisible ]
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

   // Formatowanie wyjściowe zależnie od nazwy katalogu ".." lub zwykłych nazw
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

STATIC PROCEDURE hc_drawCmdLine( pApp, aSelectedPanel, lVisiblePanels )

   LOCAL nMaxRow := sdl_MaxRow( pApp )
   LOCAL nMaxCol := sdl_MaxCol( pApp )
   LOCAL cPromptEnd
   LOCAL cCmdLine, cPadCmdLine

   // Ustawienie końcówki ;)
   IF( "Windows" $ os() )
      cPromptEnd := "> "
   ELSE
      cPromptEnd := "$ "
   ENDIF

   // Wyciągnięcie widocznej części polecenia do zmiennej cCmdLine
   cCmdLine := SubStr( aSelectedPanel[ _cCmdLine ], aSelectedPanel[ _nCmdColNo ] + 1, nMaxCol - Len( aSelectedPanel[ _cCurrentDir ] ) - 3 )

   cPadCmdLine := PadR( cCmdLine, nMaxCol )

   IF( lVisiblePanels )
      sdl_drawFont( pApp, 0, nMaxRow - 1, aSelectedPanel[ _cCurrentDir ], BLACK + "/" + GREEN )
      sdl_drawFont( pApp, Len( aSelectedPanel[ _cCurrentDir ] ), nMaxRow - 1, cPromptEnd, BLACK + "/" + CYAN )
      sdl_drawFont( pApp, Len( aSelectedPanel[ _cCurrentDir ] ) + 2, nMaxRow - 1, cPadCmdLine, BLACK + "/" + YELLOW )

      sdl_setCursorPosition( pApp, aSelectedPanel[ _nCmdCol ] + Len( aSelectedPanel[ _cCurrentDir ] ) + 2, nMaxRow - 1 )
   ELSE
      sdl_drawFont( pApp, 0, 0, aSelectedPanel[ _cCurrentDir ], BLACK + "/" + GREEN )
      sdl_drawFont( pApp, Len( aSelectedPanel[ _cCurrentDir ] ), 0, cPromptEnd, BLACK + "/" + CYAN )
      sdl_drawFont( pApp, Len( aSelectedPanel[ _cCurrentDir ] ) + 2, 0, cPadCmdLine, BLACK + "/" + YELLOW )

      sdl_setCursorPosition( pApp, aSelectedPanel[ _nCmdCol ] + Len( aSelectedPanel[ _cCurrentDir ] ) + 2, 0 )
   ENDIF

RETURN

/* -------------------------------------------------------------------------
hc_changeDir( aSelectedPanel ) --> aSelectedPanel
------------------------------------------------------------------------- */
STATIC FUNCTION hc_changeDir( aSelectedPanel )

   LOCAL nIndex, cDir, cDir0
   LOCAL nParentDirPosition

   // Ustal indeks bieżącego katalogu lub pliku
   nIndex := aSelectedPanel[ _nRowBar ] + aSelectedPanel[ _nRowNo ]

   // Sprawdzamy, czy element jest katalogiem (czy zawiera atrybut ATTR_DIRECTORY)
   IF( At( ATTR_DIRECTORY, aSelectedPanel[ _aDirList ][ nIndex ][ F_ATTR ] ) == 0 )
      RETURN aSelectedPanel
   ENDIF

   // Jeśli element to katalog "..", przechodzimy do katalogu nadrzędnego
   IF( aSelectedPanel[ _aDirList ][ nIndex ][ F_NAME ] == ".." )
      // Pełna ścieżka do bieżącego katalogu
      cDir := aSelectedPanel[ _cCurrentDir ]

      // Jeśli jesteśmy już w katalogu głównym, nie zmieniamy katalogu
      IF( cDir == hb_ps() )
         RETURN aSelectedPanel
      ENDIF

      // Nazwa katalogu nadrzędnego wyodrębniona z cDir
      cDir0 := SubStr( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) + 1 )
      cDir0 := SubStr( cDir0, 1, Len( cDir0 ) - 1 )
      // Zaktualizowana ścieżka do nadrzędnego katalogu
      cDir  := Left( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) )

      // Aktualizujemy listę katalogów i plików
      aSelectedPanel := hc_fetchList( aSelectedPanel, cDir )

      // Znajdujemy pozycję nadrzędnego katalogu w tablicy
      nParentDirPosition := Max( AScan( aSelectedPanel[ _aDirList ], { | x | x[ F_NAME ] == cDir0 } ), 1 )

      // Ustawienie pozycji w widoku panelu
      IF( nParentDirPosition > aSelectedPanel[ _nMaxRow ] - 1 )
         aSelectedPanel[ _nRowNo ]  := nParentDirPosition % ( aSelectedPanel[ _nMaxRow ] - 1 )
         aSelectedPanel[ _nRowBar ] := aSelectedPanel[ _nMaxRow ] - 1
      ELSE
         aSelectedPanel[ _nRowNo ]  := 0
         aSelectedPanel[ _nRowBar ] := nParentDirPosition
      ENDIF

   ELSE
      // Przechodzimy do wybranego katalogu
      cDir := aSelectedPanel[ _cCurrentDir ] + aSelectedPanel[ _aDirList ][ nIndex ][ F_NAME ] + hb_ps()
      aSelectedPanel[ _nRowBar ] := 1
      aSelectedPanel[ _nRowNo ]  := 0
      aSelectedPanel := hc_fetchList( aSelectedPanel, cDir )
   ENDIF

RETURN aSelectedPanel

/* *************************************************************************
Harbour C Code
************************************************************************* */
#pragma BEGINDUMP

#if defined( _WIN32 ) || defined( _WIN64 )
#include <direct.h>
#include <windows.h>
#define PATH_MAX          260  /* Windows standard path limit */
#else
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#define PATH_MAX         4096  /* Chars in a path name including nul */
#endif

// Harbour
#include <hbapi.h>

typedef enum _bool bool;
enum _bool
{
   F = 0,
   T = ( ! 0 )
};

// bool hc_isTogglePanels( bool visiblePanels )
HB_FUNC( HC_ISTOGGLEPANELS )
{
   bool visiblePanels = hb_parl( 1 );
   visiblePanels = !visiblePanels;
   hb_retl( visiblePanels );
}

// bool hc_isExecutable( const char *commandLine )
HB_FUNC( HC_ISEXECUTABLE )
{
   const char *commandLine = hb_parc( 1 );

#if defined( _WIN32 ) || defined( _WIN64 )

   const char *extensions[] = { ".exe", ".bat", ".com", ".cmd" };
   const char *ext = strrchr( commandLine, '.' ); // Znajdź ostatnie wystąpienie '.'

   if( ext )
   {
      for( size_t i = 0; i < sizeof( extensions ) / sizeof( extensions[ 0 ] ); i++ )
      {
         if( strcmp( ext, extensions[ i ] ) == 0 )
         {
            hb_retl( T );
            return;
         }
      }
   }
   hb_retl( F );
   return;

#else
   const int commandBufferSize = PATH_MAX;
   const int commandPrefixMaxSize = 10;

   if( strlen( commandLine ) > ( commandBufferSize - commandPrefixMaxSize - 1 ) )
   {
      fprintf( stderr, "Command is too long\n" );
      hb_retl( F );
      return;
   }

   char command[ commandBufferSize + 50 ];
   snprintf( command, sizeof( command ), "file --mime-type -b %s", commandLine );

   FILE *fp = popen( command, "r" );
   if( fp == NULL )
   {
      perror( "popen" );
      hb_retl( F );
      return;
   }

   char mimeType[ 100 ];
   if( fgets( mimeType, sizeof( mimeType ), fp ) )
   {
      mimeType[ strcspn( mimeType, "\n" ) ] = 0;  // Usuń znak nowej linii z `fgets`

      if( strcmp( mimeType, "application/x-executable" ) == 0 || strcmp( mimeType, "application/x-pie-executable" ) == 0 )
      {
         pclose( fp );
         hb_retl( T );
         return;
      }
   }

   pclose( fp );
   hb_retl( F );
   return;

#endif
}

// bool hc_runApp( const char *commandLine )
HB_FUNC( HC_RUNAPP )
{
   const char *commandLine = hb_parc( 1 );

#if defined( _WIN32 ) || defined( _WIN64 )
   char commandWithQuotes[ PATH_MAX ];
   snprintf( commandWithQuotes, sizeof( commandWithQuotes ), "\"%s\"", commandLine );

   HINSTANCE result = ShellExecute( NULL, "open", commandWithQuotes, NULL, NULL, SW_SHOWNORMAL );
   if( ( uintptr_t ) result <= 32 )
   {
      fprintf( stderr, "Failed to run application: %s\n", commandLine );
      hb_retl( F );
      return;
   }
   hb_retl( T );
   return;
#else
   const int commandBufferSize = PATH_MAX;
   const int commandPrefixMaxSize = 10;

   if( strlen( commandLine ) > ( commandBufferSize - commandPrefixMaxSize - 1 ) )
   {
      fprintf( stderr, "Command is too long\n" );
      hb_retl( F );
      return;
   }

   char command[ commandBufferSize ];
   snprintf( command, sizeof( command ), "%s &", commandLine );

   int result = system( command );
   if( result != 0 )
   {
      fprintf( stderr, "Failed to run executable: %s\n", commandLine );
      hb_retl( F );
      return;
   }
   hb_retl( T );
   return;
#endif
}

// bool hc_openFile( const char *commandLine )
HB_FUNC( HC_OPENFILE )
{
   const char *commandLine = hb_parc( 1 );

#if defined( _WIN32 ) || defined( _WIN64 )
   char commandWithQuotes[ PATH_MAX ];
   snprintf( commandWithQuotes, sizeof( commandWithQuotes ), "\"%s\"", commandLine );

   HINSTANCE result = ShellExecute( NULL, "open", commandWithQuotes, NULL, NULL, SW_SHOWNORMAL );
   if( ( uintptr_t ) result <= 32 )
   {
      fprintf( stderr, "Failed to open file: %s\n", commandLine );
      hb_retl( F );
      return;
   }
   hb_retl( T );
   return;
#else
   const int commandBufferSize = PATH_MAX;
   const int commandPrefixMaxSize = 10;

   if( strlen( commandLine ) > ( commandBufferSize - commandPrefixMaxSize - 1 ) )
   {
      fprintf( stderr, "Command is too long\n" );
      hb_retl( F );
      return;
   }

   char command[ commandBufferSize ];
   snprintf( command, sizeof( command ), "xdg-open %s", commandLine );

   int result = system( command );
   if( result != 0 )
   {
      fprintf( stderr, "Failed to open file: %s\n", commandLine );
      hb_retl( F );
      return;
   }
   hb_retl( T );
   return;
#endif
}

// bool hc_chDir( const char *path )
HB_FUNC( HC_CHDIR )
{
   const char *path = hb_parc( 1 );

   if( path == NULL )
   {
      fprintf( stderr, "Error: Path is NULL.\n" );
      hb_retl( F );
      return;
   }

#if defined( _WIN32 ) || defined( _WIN64 )
   if( !SetCurrentDirectory( path ) )
   {
      fprintf( stderr, "Error: Could not change directory to '%s'.\n", path );
      hb_retl( F );
      return;
   }
#else
   if( chdir( path ) != 0 )
   {
      perror( "Error changing directory" );
      hb_retl( F );
      return;
   }
#endif

   hb_retl( T );  // Zwracamy T, jeśli zmiana katalogu się powiodła
   return;
}

// char *hc_executeAndCapture( const char *command )
HB_FUNC( HC_EXECUTEANDCAPTURE )
{
   const char *command = hb_parc( 1 );

   if( command == NULL || strlen( command ) == 0 )
   {
      fprintf( stderr, "Error: Command is empty.\n" );
      hb_retc( "" );
      return;
   }

   char commandWithErrorCapture[ PATH_MAX ];
   snprintf( commandWithErrorCapture, sizeof( commandWithErrorCapture ), "%s 2>&1", command );

   // Otwórz proces i przechwyć wynik
   FILE *fp = popen( commandWithErrorCapture, "r" );
   if( fp == NULL )
   {
      fprintf( stderr, "Failed to run command: %s\n", command );
      hb_retc( "" );
      return;
   }

   // Bufor dla przechwytywania wyników
   char result[ 256 ];
   // Dynamiczna alokacja na pełny wynik
   size_t totalLength = 0;
   char *fullOutput = hb_xgrab( 1 );
   fullOutput[ 0 ] = '\0';

   // Czytaj linia po linii i przechwyć cały wynik
   while( fgets( result, sizeof( result ), fp ) != NULL )
   {
      size_t lineLength = strlen( result );
      char *newOutput = hb_xrealloc( fullOutput, totalLength + lineLength + 1 );

      if( newOutput == NULL )
      {
         fprintf( stderr, "Memory allocation error.\n" );
         hb_xfree( fullOutput );
         pclose( fp );
         hb_retc( "" );
         return;
      }

      fullOutput = newOutput;
      strcat( fullOutput, result );
      totalLength += lineLength;
   }

   pclose( fp );

   hb_retc( fullOutput );

   hb_xfree( fullOutput );
}

#pragma ENDDUMP
/* *************************************************************************
End Harbour C Code
************************************************************************* */

