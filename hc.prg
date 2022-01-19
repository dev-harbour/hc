/*
 *
 */

#include "hbraylib.ch"

PROCEDURE Main()

   LOCAL nScreenWidth  := 80
   LOCAL nScreenHeight := 25

   SetConfigFlags( hb_bitOr( FLAG_WINDOW_RESIZABLE, FLAG_VSYNC_HINT ) )

   hb_InitWindow( nScreenWidth, nScreenHeight, "Harbour Commander" )
   SetWindowIcon( LoadImage( "docs/assets/img/hc.png" ) )

   SetTargetFPS( 60 )

   DO WHILE( ! WindowShouldClose() )

      BeginDrawing()

         hb_ClearBackground( 0xFFFFFF )

      EndDrawing()

   ENDDO

   CloseWindow()

   RETURN
