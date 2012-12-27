/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2012-2013 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#include "hbtoqt.ch"
#include "hbqtstd.ch"
#include "hbqtgui.ch"
#include "inkey.ch"
#include "error.ch"
#include "hbclass.ch"

#include "hbtrace.ch"


#define __ev_mousepress_on_frozen__               31               /* Mousepress on Frozen */
#define __ev_mousepress__                         2                /* Mousepress */
#define __ev_xbpBrw_itemSelected__                3                /* xbeBRW_ItemSelected */
#define __ev_wheel__                              4                /* wheelEvent */
#define __ev_horzscroll_via_qt__                  11               /* Horizontal Scroll Position : sent by Qt */
#define __ev_vertscroll_via_user__                101              /* Vertical Scrollbar Movements by the User */
#define __ev_vertscroll_sliderreleased__          102              /* Vertical Scrollbar: Slider Released */
#define __ev_horzscroll_slidermoved__             103              /* Horizontal Scrollbar: Slider moved */
#define __ev_horzscroll_sliderreleased__          104              /* Horizontal Scrollbar: Slider Released */
#define __ev_columnheader_pressed__               111              /* Column Header Pressed */
#define __ev_headersec_resized__                  121              /* Header Section Resized */
#define __ev_footersec_resized__                  122              /* Footer Section Resized */

#define __ev_tableViewBlock_main__                1501
#define __ev_tableViewBlock_left__                1502
#define __ev_tableViewBlock_right__               1503


#define HBQTBRW_CURSOR_NONE                       1
#define HBQTBRW_CURSOR_CELL                       2
#define HBQTBRW_CURSOR_ROW                        3

#define HBQTCOL_TYPE_ICON                         1
#define HBQTCOL_TYPE_BITMAP                       2
#define HBQTCOL_TYPE_SYSICON                      3
#define HBQTCOL_TYPE_TEXT                         4
#define HBQTCOL_TYPE_FILEICON                     5
#define HBQTCOL_TYPE_FILEMINIICON                 6
#define HBQTCOL_TYPE_MULTILINETEXT                7

#define ISFROZEN( n )                             ( ascan( ::aLeftFrozen, n ) > 0 .OR. ascan( ::aRightFrozen, n ) > 0 )


STATIC PROCEDURE _GENLIMITRTE()
   LOCAL oError := ErrorNew()

   oError:severity    := ES_ERROR
   oError:genCode     := EG_LIMIT
   oError:subSystem   := "TBROWSE"
   oError:subCode     := 0
   oError:description := hb_LangErrMsg( EG_LIMIT )
   oError:canRetry    := .F.
   oError:canDefault  := .F.
   oError:fileName    := ""
   oError:osCode      := 0

   Eval( ErrorBlock(), oError )
   __errInHandler()

   RETURN


FUNCTION HbQtBrowseNew( nTop, nLeft, nBottom, nRight, oParent, oFont )
   RETURN HbQtBrowse():new( nTop, nLeft, nBottom, nRight, oParent, oFont )


CLASS HbQtBrowse INHERIT TBrowse

   DATA   oWidget

   /* Overloaded Methods */
   METHOD new( nTop, nLeft, nBottom, nRight, oParent, oFont )
   METHOD doConfigure()
   METHOD rowCount()
   METHOD up()
   METHOD down()
   METHOD pageUp()
   METHOD pageDown()
   METHOD goTop()
   METHOD goBottom()
   METHOD left()
   METHOD right()
   METHOD firstCol()
   METHOD lastCol()
   METHOD home()
   METHOD end()
   METHOD panHome()
   METHOD panEnd()
   METHOD panLeft()
   METHOD panRight()
   METHOD refreshAll()
   METHOD refreshCurrent()

   ACCESS freeze                                  METHOD getFrozen            // get number of frozen columns
   ASSIGN freeze                                  METHOD freeze               // set number of columns to freeze

   /* HbQt Extentions */
   METHOD navigationBlock( bBlock )               SETGET

   /* Xbase++ */
   METHOD firstPosBlock( bBlock )                 SETGET
   METHOD lastPosBlock( bBlock )                  SETGET
   METHOD phyPosBlock( bBlock )                   SETGET
   METHOD posBlock( bBlock )                      SETGET
   METHOD goPosBlock( bBlock )                    SETGET
   METHOD hitBottomBlock( bBlock )                SETGET
   METHOD hitTopBlock( bBlock )                   SETGET
   METHOD stableBlock( bBlock )                   SETGET

   ACCESS rFreeze                                 METHOD getRFrozen           // get number of frozen columns
   ASSIGN rFreeze                                 METHOD rFreeze              // set number of columns to freeze at right side

   METHOD moveLeft()
   METHOD moveRight()
   METHOD moveHome()
   METHOD moveEnd()

   METHOD horizontalScrollbar                     SETGET
   METHOD verticalScrollbar                       SETGET
   METHOD cursorMode                              SETGET

   METHOD editCell()                              INLINE ::oTableView:edit( ::getCurrentIndex() )
   METHOD edit( cTitle, lSaveOnLastGet, lDownAfterSave )
   METHOD editBlock( bBlock )                     SETGET
   METHOD search( xValue )
   METHOD print( cPrinter, lOpenPrintDialog )
   METHOD searchBlock( bBlock )                   SETGET
   METHOD contextMenuBlock( bBlock )              SETGET
   ACCESS toolbar                                 INLINE ::oToolBar:isVisible()
   ASSIGN toolbar                                 METHOD manageToolbar
   ACCESS statusbar                               INLINE ::oStatusBar:isVisible()
   ASSIGN statusbar                               METHOD manageStatusbar
   ASSIGN statusMessage( cMessage )               INLINE iif( Empty( cMessage ), ::oStatusBar:clearMessage(), ::oStatusBar:showMessage( cMessage ) )
   ACCESS title                                   METHOD getTitle
   ASSIGN title                                   METHOD setTitle
   ACCESS editable                                METHOD getEditable
   ASSIGN editable                                METHOD setEditable

PROTECTED:

   METHOD cellValue( nRow, nCol )                /* Overloaded */

   /* HbQt Internal Methods */
   METHOD cellValueA( nRow, nCol )
   METHOD create()
   METHOD execSlot( nEvent, p1, p2, p3 )
   METHOD supplyInfo( nMode, nCall, nRole, nX, nY )
   METHOD compatColor( nColor )
   METHOD compatIcon( cIcon )
   METHOD compatBrush( nColor )

   DATA   oParent
   DATA   oFont

   DATA   oToolbar

   DATA   oDbfModel
   DATA   oModelIndex                             INIT   QModelIndex()
   DATA   oVHeaderView
   DATA   oHeaderView
   DATA   oVScrollBar
   DATA   oHScrollBar
   DATA   oViewport
   DATA   pCurIndex

   DATA   lFirst                                  INIT   .t.
   DATA   nRowsInView                             INIT   1

   METHOD connect()

   METHOD setHorzOffset()
   METHOD setVertScrollBarRange( lPageStep )
   METHOD setHorzScrollBarRange( lPageStep )
   METHOD updateVertScrollBar()
   METHOD updatePosition()

   DATA   lHScroll                                INIT   .F.
   DATA   lVScroll                                INIT   .F.
   DATA   nCursorMode                             INIT   0

   DATA   lSizeCols                               INIT   .T.
   METHOD sizeCols                                SETGET

   DATA   softTrack                               INIT   .T.
   DATA   nHorzOffset                             INIT   -1
   DATA   lReset                                  INIT   .F.
   DATA   lHorzMove                               INIT   .f.

   DATA   oTableView
   DATA   oGridLayout
   DATA   oFooterView
   DATA   oFooterModel

   DATA   oLeftView
   DATA   oLeftVHeaderView
   DATA   oLeftHeaderView
   DATA   oLeftFooterView
   DATA   oLeftFooterModel
   DATA   oLeftDbfModel

   DATA   oRightView
   DATA   oRightVHeaderView
   DATA   oRightHeaderView
   DATA   oRightFooterView
   DATA   oRightFooterModel
   DATA   oRightDbfModel

   METHOD buildLeftFreeze()
   METHOD buildRightFreeze()
   METHOD fetchColumnInfo( nCall, nRole, nArea, nRow, nCol )

   METHOD setLeftFrozen( aColFrozens )
   METHOD setRightFrozen( aColFrozens )
   DATA   aLeftFrozen                             INIT   {}
   DATA   aRightFrozen                            INIT   {}
   DATA   nLeftFrozen                             INIT   0
   DATA   nRightFrozen                            INIT   0

   DATA   gridStyle                               INIT   Qt_SolidLine

   DATA   nCellHeight                             INIT   20
   DATA   oDefaultCellSize
   METHOD setCellHeight( nCellHeight )
   METHOD setCurrentIndex( lReset )
   METHOD setIndex( qModelIndex )                 INLINE ::oTableView:setCurrentIndex( qModelIndex )
   METHOD getCurrentIndex()                       INLINE ::oDbfModel:index( ::rowPos - 1, ::colPos - 1 )
   ACCESS getDbfModel()                           INLINE ::oDbfModel
   METHOD openPersistentEditor()
   METHOD setFocus()                              INLINE ::oTableView:setFocus()

   DATA   qDelegate

   METHOD manageKeyPress( oEvent )
   METHOD manageFrameResized()
   METHOD manageMouseDblClick( oMouseEvent )
   METHOD manageMousePress( oMouseEvent )
   METHOD manageMouseRelease( oMouseEvent )
   METHOD manageMouseWheel( oWheelEvent )
   METHOD manageCommitData( qWidget )
   METHOD manageEditorClosed( pWidget, nHint )
   METHOD manageScrollContents( nX, nY )
   METHOD manageColumnMoved( nLogicalIndex, nOldVisualIndex, nNewVisualIndex )

   DATA   hColors                                 INIT {=>}
   DATA   hIcons                                  INIT {=>}
   DATA   hBrushes                                INIT {=>}

   DATA   aCellValuesA  AS ARRAY                  INIT {}   // cell values buffers for each record - actual

   DATA   bNavigationBlock                        INIT NIL
   DATA   bSearchBlock                            INIT NIL
   DATA   bEditBlock                              INIT NIL
   DATA   bFirstPosBlock                          INIT NIL
   DATA   bLastPosBlock                           INIT NIL
   DATA   bPhyPosBlock                            INIT NIL
   DATA   bPosBlock                               INIT NIL
   DATA   bGoPosBlock                             INIT NIL
   DATA   bHitBottomBlock                         INIT NIL
   DATA   bHitTopBlock                            INIT NIL
   DATA   bStableBlock                            INIT NIL

   METHOD skipRows( nRows )                                 // INTERNAL - skips <nRows> back or forward : Resizing
   METHOD skipCols( nCols )                                 // INTERNAL - skips <nCols> right or left   : Resizing

   /* Editor specific calls */
   METHOD loadRow()
   METHOD saveRow()
   METHOD populateGets()
   METHOD mangageEditorKeyPress( oKeyEvent )
   //
   DATA   aOriginal
   DATA   aModified
   DATA   aCaptions
   DATA   lSaveOnLastGet
   DATA   lDownAfterSave
   DATA   aGetList
   DATA   aPosSize
   DATA   lEditable

   METHOD execSearch()
   //
   DATA   oStatusBar
   DATA   oSearchGet
   DATA   xSearchValue
   DATA   aSearchList

   METHOD manageContextMenu( oPos )
   //
   DATA   bContextMenuBlock
   DATA   oContextMenu

   METHOD buildActions()
   //
   DATA   oActSave
   DATA   oActEdit
   DATA   oActPrint
   DATA   oActSearch
   //
   DATA   oActGoUp
   DATA   oActGoTop
   DATA   oActGoDown
   DATA   oActGoBottom
   //
   DATA   oActPanHome
   DATA   oActLeft
   DATA   oActRight
   DATA   oActPanEnd
   //
   DATA   oActMoveToLeft
   DATA   oActMoveToRight
   DATA   oActMoveToFirst
   DATA   oActMoveToLast
   //
   DATA   oActToColumn
   DATA   oActToColumnM
   DATA   oComboColumn
   DATA   oComboColumnM

   DATA   oActFreezeLPlus
   DATA   oActFreezeLMinus
   DATA   oActFreezeRPlus
   DATA   oActFreezeRMinus

   METHOD buildToolbar()
   METHOD toColumn( cColumn )

   METHOD printPreview( oPrinter )
   METHOD paintRequested( oPrinter )
   METHOD printReport( oPrinter )

   METHOD getTitle()
   METHOD setTitle( xTitle )
   METHOD drawTitle( oPainter, aTitle, nT, nRH, nCols, nPage, nML, aX, nF, aLen, nPxlX, nPxlW, nM, nAW, nLeading )
   DATA   xTitle
   DATA   oPenBlack
   DATA   oPrinter

   ENDCLASS


METHOD HbQtBrowse:new( nTop, nLeft, nBottom, nRight, oParent, oFont )

   ::TBrowse:new( nTop, nLeft, nBottom, nRight )

   hb_default( @oFont, QFont( "Courier new", 10 ) )

   ::oParent := oParent
   ::oFont   := oFont

   ::colorSpec := "N/W*, N/W"                     /* DEFAULT TO GUI Colors */

   ::create()

   RETURN Self


METHOD HbQtBrowse:create()

   ::oWidget := QFrame( ::oParent )

   /* Important here as other parts will be based on it*/
   ::oWidget:resize( ::oParent:width(), ::oParent:height() )

   /* Toolbar actions */
   ::buildActions()

   /* .DBF Manipulation Model */
   ::oDbfModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 141, t, role, x, y ) } )

   /* Subclass of QTableView */
   WITH OBJECT ::oTableView := HBQTableView()
      /* Set block to receive protected information */
      :hbSetBlock( {|p,p1,p2| ::execSlot( __ev_tableViewBlock_main__, p, p1, p2 ) } )
      /* Some parameters */
      :setFont( ::oFont )
      :setTabKeyNavigation( .t. )
      :setShowGrid( .t. )
      :setGridStyle( ::gridStyle )                /* to be based on column definition */
      :setSelectionMode( QAbstractItemView_SingleSelection )
      :setSelectionBehavior( iif( ::cursorMode == HBQTBRW_CURSOR_ROW, QAbstractItemView_SelectRows, QAbstractItemView_SelectItems ) )
      :setAlternatingRowColors( .t. )
      :setContextMenuPolicy( Qt_CustomContextMenu )
      :setEditTriggers( QAbstractItemView_AnyKeyPressed )
      /* Finetune Horizontal Scrollbar */
      :setHorizontalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
      /* Replace Vertical Scrollbar with our own */
      :setVerticalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
      /* Veritical Header because of Performance boost */
      :verticalHeader():hide()
      /* Attach Model with the View */
      :setModel( ::oDbfModel )
   ENDWITH

   ::oHScrollBar := QScrollBar()
   ::oHScrollBar:setOrientation( Qt_Horizontal )

   ::oVScrollBar := QScrollBar()
   ::oVScrollBar:setOrientation( Qt_Vertical )

   /*  Horizontal Header Fine Tuning */
   WITH OBJECT ::oHeaderView := ::oTableView:horizontalHeader()
      :setHighlightSections( .F. )
      :setMovable( .T. )                /* Needs more time TO investigae Qt behvior - first efforts have been futile */
   ENDWITH

   /*  Horizontal Footer */
   ::oFooterModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 142, t, role, x, y ) } )
   //
   WITH OBJECT ::oFooterView := QHeaderView( Qt_Horizontal )
      :setHighlightSections( .F. )
      :setMinimumHeight( 20 )
      :setMaximumHeight( 20 )
      :setResizeMode( QHeaderView_Fixed )
      :setFocusPolicy( Qt_NoFocus )
      :setModel( ::oFooterModel )
      :hide()
   ENDWITH

   /*  Widget for ::setLeftFrozen( aColumns )  */
   ::buildLeftFreeze()
   /*  Widget for ::setRightFrozen( aColumns )  */
   ::buildRightFreeze()

   /* Toolbar hosting navigational actions */
   ::buildToolbar()

   WITH OBJECT ::oStatusBar := QStatusBar( ::oWidget )
//    :setFont( ::oFont )                         /* Should we keep it as is ? */
      :hide()
   ENDWITH

   /* Place all widgets in a Grid Layout */
   WITH OBJECT ::oGridLayout := QGridLayout( ::oWidget )
      :setContentsMargins( 0,0,0,0 )
      :setHorizontalSpacing( 0 )
      :setVerticalSpacing( 0 )

      :addWidget( ::oToolbar        , 0, 0, 1, 4 )

      :addWidget( ::oLeftView       , 1, 0, 1, 1 )
      :addWidget( ::oLeftFooterView , 2, 0, 1, 1 )

      :addWidget( ::oTableView      , 1, 1, 1, 1 )
      :addWidget( ::oFooterView     , 2, 1, 1, 1 )

      :addWidget( ::oRightView      , 1, 2, 1, 1 )
      :addWidget( ::oRightFooterView, 2, 2, 1, 1 )

      :addWidget( ::oHScrollBar     , 3, 0, 1, 3 )

      :addWidget( ::oVScrollBar     , 1, 3, 2, 1 )
      :addWidget( ::oStatusBar      , 4, 0, 1, 4 )
   ENDWITH

   ::oWidget:show()

   /* Viewport */
   ::oViewport := ::oTableView:viewport()
#if 0
   /* Why we need it ? */
   ::oWidget:setGeometry( ::oWidget:geometry() )
#endif
   /* Handle the delegate - but how TO ? */
   ::qDelegate := QItemDelegate()
   ::oTableView:setItemDelegate( ::qDelegate )

   ::connect()

   RETURN Self


METHOD HbQtBrowse:doConfigure()     /* Overloaded */

   LOCAL aCol, aVal, aValA, nColCount, nRowCount, nHeight, oCol
   LOCAL nViewH, i, xVal, oFontMetrics, n, nLeftWidth, nwVal, nwHead
   LOCAL nMaxCellH, lShowFooter

   ::TBrowse:doConfigure()

   IF HB_ISOBJECT( ::oHScrollBar )
      IF ! ::lHScroll
         ::oHScrollBar:hide()
      ELSE
         ::oHScrollBar:show()
      ENDIF
   ENDIF
   IF HB_ISOBJECT( ::oVScrollBar )
      IF ! ::lVScroll
         ::oVScrollBar:hide()
      ELSE
         ::oVScrollBar:show()
      ENDIF
   ENDIF

   lShowFooter := .F.
   FOR i := 1 TO ::colCount
      IF ! Empty( ::columns[ i ]:footing )
         lShowFooter := .T.
      ENDIF
   NEXT
   IF lShowFooter
      IF HB_ISOBJECT( ::oLeftFooterView )
         ::oLeftFooterView:show()
      ENDIF
      IF HB_ISOBJECT( ::oRightFooterView )
         ::oRightFooterView:show()
      ENDIF
      IF HB_ISOBJECT( ::oFooterView )
         ::oFooterView:show()
      ENDIF
   ELSE
      IF HB_ISOBJECT( ::oLeftFooterView )
         ::oLeftFooterView:hide()
      ENDIF
      IF HB_ISOBJECT( ::oRightFooterView )
         ::oRightFooterView:hide()
      ENDIF
      IF HB_ISOBJECT( ::oFooterView )
         ::oFooterView:hide()
      ENDIF
   ENDIF

   ::oTableView:setSelectionBehavior( iif( ::cursorMode == HBQTBRW_CURSOR_ROW, QAbstractItemView_SelectRows, QAbstractItemView_SelectItems ) )

   /* Calculate how many rows fit in the view */
   IF len( ::columns ) > 0
      oFontMetrics := QFontMetrics( ::oTableView:font() )
      nHeight := oFontMetrics:height() + 3

      nMaxCellH := nHeight + 5
      //aeval( ::columns, {|o| nMaxCellH := max( nMaxCellH, o:hHeight ) } )
      //
      ::oHeaderView:setMaximumHeight( nMaxCellH )
      ::oHeaderView:setMinimumHeight( nMaxCellH )
      //
      ::oLeftHeaderView:setMaximumHeight( nMaxCellH )
      ::oLeftHeaderView:setMinimumHeight( nMaxCellH )
      //
      ::oRightHeaderView:setMaximumHeight( nMaxCellH )
      ::oRightHeaderView:setMinimumHeight( nMaxCellH )

      nMaxCellH := nHeight + 5
      //aeval( ::columns, {|o| nMaxCellH := max( nMaxCellH, o:fHeight ) } )
      //
      ::oFooterView     :setMaximumHeight( nMaxCellH )
      ::oLeftFooterView :setMaximumHeight( nMaxCellH )
      ::oRightFooterView:setMaximumHeight( nMaxCellH )

      nMaxCellH := nHeight
      //aeval( ::columns, {|o| nMaxCellH := max( nMaxCellH, o:dHeight ) } )
      //
      nViewH := ::oViewport:height() //- ::oHeaderView:height()
      ::nRowsInView := Int( nViewH / nMaxCellH )
      IF ( nViewH % nMaxCellH ) > ( nMaxCellH / 2 )
         ::nRowsInView++
      ENDIF

      /* Probably this is the appropriate time to update row heights */
      ::nCellHeight := nMaxCellH
      ::setCellHeight( nMaxCellH )

      /* Implement Column Resizing Mode */
      ::oHeaderView:setResizeMode( iif( ::lSizeCols, QHeaderView_Interactive, QHeaderView_Fixed ) )
      ::oFooterView:setResizeMode( QHeaderView_Fixed )
      //
      ::oLeftHeaderView:setResizeMode( QHeaderView_Fixed )
      ::oLeftFooterView:setResizeMode( QHeaderView_Fixed )
      //
      ::oRightHeaderView:setResizeMode( QHeaderView_Fixed )
      ::oRightFooterView:setResizeMode( QHeaderView_Fixed )

      /* Set column widths */
      FOR i := 1 TO len( ::columns )
         IF ::columns[ i ]:nColWidth != NIL
            ::oHeaderView:resizeSection( i-1, ::columns[ i ]:nColWidth )
            ::oFooterView:resizeSection( i-1, ::columns[ i ]:nColWidth )
         ELSE
            xVal := transform( eval( ::columns[ i ]:block ), ::columns[ i ]:picture )

            nwVal := oFontMetrics:width( xVal, -1 )
            nwHead := oFontMetrics:width( ::columns[ i ]:heading(), -1 )

            ::columns[ i ]:nColWidth := max( nwVal, nwHead ) + 8

            ::oHeaderView:resizeSection( i-1, max( nwVal, nwHead ) + 8 )
            ::oFooterView:resizeSection( i-1, max( nwVal, nwHead ) + 8 )
         *  ::oHeaderView:resizeSection( i-1, oFontMetrics:width( xVal, -1 ) + 8 )
         *  ::oFooterView:resizeSection( i-1, oFontMetrics:width( xVal, -1 ) + 8 )
         ENDIF
      NEXT

      nLeftWidth := 0
      FOR n := 1 TO ::nLeftFrozen
         i := ::aLeftFrozen[ n ]
         IF ::columns[ i ]:nColWidth != NIL
            ::oLeftHeaderView:resizeSection( n-1, ::columns[ i ]:nColWidth )
            ::oLeftFooterView:resizeSection( n-1, ::columns[ i ]:nColWidth )
         ELSE
            xVal := transform( eval( ::columns[ i ]:block ), ::columns[ i ]:picture )
            nwVal := oFontMetrics:width( xVal, -1 )
            nwHead := oFontMetrics:width( ::columns[ i ]:heading(), -1 )
            ::oLeftHeaderView:resizeSection( n-1, max( nwVal, nwHead ) + 8 )
            ::oLeftFooterView:resizeSection( n-1, max( nwVal, nwHead ) + 8 )
         /* ::oLeftHeaderView:resizeSection( n-1, oFontMetrics:width( xVal, -1 ) + 8 ) */
         /* ::oLeftFooterView:resizeSection( n-1, oFontMetrics:width( xVal, -1 ) + 8 ) */
         ENDIF
         nLeftWidth += ::oLeftHeaderView:sectionSize( n-1 )
      NEXT
      ::oLeftView:setFixedWidth( 4 + nLeftWidth )
      ::oLeftFooterView:setFixedWidth( 4 + nLeftWidth )

      nLeftWidth := 0
      FOR n := 1 TO ::nRightFrozen
         i := ::aRightFrozen[ n ]
         IF ::columns[ i ]:nColWidth != NIL
            ::oRightHeaderView:resizeSection( n-1, ::columns[ i ]:nColWidth )
            ::oRightFooterView:resizeSection( n-1, ::columns[ i ]:nColWidth )
         ELSE
            xVal := transform( eval( ::columns[ i ]:block ), ::columns[ i ]:picture )
            nwVal := oFontMetrics:width( xVal, -1 )
            nwHead := oFontMetrics:width( ::columns[ i ]:heading(), -1 )
            ::oRightHeaderView:resizeSection( n-1, max( nwVal, nwHead ) + 8 )
            ::oRightFooterView:resizeSection( n-1, max( nwVal, nwHead ) + 8 )
         /* ::oRightHeaderView:resizeSection( n-1, oFontMetrics:width( xVal, -1 ) + 8 ) */
         /* ::oRightFooterView:resizeSection( n-1, oFontMetrics:width( xVal, -1 ) + 8 ) */
         ENDIF
         nLeftWidth += ::oRightHeaderView:sectionSize( n-1 )
      NEXT
      ::oRightView:setFixedWidth( 4 + nLeftWidth )
      ::oRightFooterView:setFixedWidth( 4 + nLeftWidth )
   ENDIF

   IF ::nLeftFrozen == 0 .AND. HB_ISOBJECT( ::oLeftView )
      ::oLeftView:hide()
   ELSEIF ::nLeftFrozen > 0 .AND. HB_ISOBJECT( ::oLeftView )
      ::oLeftView:show()
   ENDIF

   IF ::nRightFrozen == 0 .AND. HB_ISOBJECT( ::oRightView )
      ::oRightView:hide()
   ELSEIF ::nRightFrozen > 0 .AND. HB_ISOBJECT( ::oRightView )
      ::oRightView:show()
   ENDIF

   FOR i := 1 TO ::colCount
      IF ISFROZEN( i )
         ::oTableView:hideColumn( i - 1 )
         ::oFooterView:setSectionHidden( i - 1, .T. )
      ELSE
         ::oTableView:showColumn( i - 1 )
         ::oFooterView:setSectionHidden( i - 1, .F. )
      ENDIF
   NEXT

   nColCount := ::colCount
   nRowCount := ::rowCount
   IF nRowCount == 0
      _GENLIMITRTE()
   ENDIF

   /* create new record buffer */
   ASize( ::aCellStatus , nRowCount )
   ASize( ::aDispStatus , nRowCount )
   ASize( ::aCellValues , nRowCount )
   ASize( ::aCellValuesA, nRowCount )
   ASize( ::aCellColors , nRowCount )
   AFill( ::aCellStatus , .F. )
   AFill( ::aDispStatus , .T. )
   FOR EACH aVal, aValA, aCol IN ::aCellValues, ::aCellValuesA, ::aCellColors
      IF aVal == NIL
         aVal := Array( nColCount )
      ELSE
         ASize( aVal, nColCount )
      ENDIF
      IF aValA == NIL
         aValA := Array( nColCount )
      ELSE
         ASize( aValA, nColCount )
      ENDIF
      IF aCol == NIL
         aCol := Array( nColCount )
      ELSE
         ASize( aCol, nColCount )
      ENDIF
   NEXT

   ::lStable     := .F.
   ::lFrames     := .T.
   ::nLastRow    := nRowCount
   ::nLastScroll := 0
   ::nLastPos    := 0
   IF ::nRowPos > nRowCount
      ::nRowPos := nRowCount
   ELSEIF ::nRowPos < 1
      ::nRowPos := 1
   ENDIF

   ::setHorzScrollBarRange()

   ::setCellHeight( ::nCellHeight )

   /* Tell Qt to Reload Everything */
   ::oDbfModel:reset()
   //
   IF HB_ISOBJECT( ::oLeftDbfModel )
      ::oLeftDbfModel:reset()
   ENDIF
   IF HB_ISOBJECT( ::oRightDbfModel )
      ::oRightDbfModel:reset()
   ENDIF

   ::oComboColumn:clear()
   ::oComboColumnM:clear()
   FOR EACH oCol IN ::columns
      ::oComboColumn:addItem( oCol:heading )
      ::oComboColumnM:addItem( oCol:heading )
   NEXT

   RETURN Self


METHOD HbQtBrowse:connect()

   ::oTableView       : connect( QEvent_KeyPress                     , {|oKeyEvent| ::manageKeyPress( oKeyEvent )                      } )
   ::oWidget          : connect( QEvent_Resize                       , {|         | ::manageFrameResized(), .T.                        } )

   ::oLeftHeaderView  : connect( "sectionPressed(int)"               , {|i      | ::execSlot( __ev_mousepress_on_frozen__     , i    ) } )
   ::oLeftFooterView  : connect( "sectionPressed(int)"               , {|i      | ::execSlot( __ev_mousepress_on_frozen__     , i    ) } )

   ::oRightHeaderView : connect( "sectionPressed(int)"               , {|i      | ::execSlot( __ev_mousepress_on_frozen__     , i    ) } )
   ::oRightFooterView : connect( "sectionPressed(int)"               , {|i      | ::execSlot( __ev_mousepress_on_frozen__     , i    ) } )

   ::oTableView       : connect( "customContextMenuRequested(QPoint)", {|p      | ::manageContextMenu( p )                             } )

   ::oHScrollBar      : connect( "actionTriggered(int)"              , {|i      | ::execSlot( __ev_horzscroll_slidermoved__   , i    ) } )
   ::oHScrollBar      : connect( "sliderReleased()"                  , {|i      | ::execSlot( __ev_horzscroll_sliderreleased__, i    ) } )

   ::oVScrollBar      : connect( "actionTriggered(int)"              , {|i      | ::execSlot( __ev_vertscroll_via_user__      , i    ) } )
   ::oVScrollBar      : connect( "sliderReleased()"                  , {|i      | ::execSlot( __ev_vertscroll_sliderreleased__, i    ) } )
#if 0
   ::oHeaderView      : connect( "sectionPressed(int)"               , {|i      | ::execSlot( __ev_columnheader_pressed__     , i    ) } )
   ::oHeaderView      : connect( "sectionResized(int,int,int)"       , {|i,i1,i2| ::execSlot( __ev_headersec_resized__   , i, i1, i2 ) } )
#endif
   ::oHeaderView      : connect( "sectionMoved(int,int,int)"         , {|i,i1,i2| ::manageColumnMoved( i, i1, i2 )                     } )

   ::qDelegate        : connect( "commitData(QWidget*)"              , {|p      | ::manageCommitData( p )                              } )
   ::qDelegate        : connect( "closeEditor(QWidget*,QAbstractItemDelegate::EndEditHint)", {|p,p1 | ::manageEditorClosed( p, p1 )    } )

   RETURN Self


METHOD HbQtBrowse:buildLeftFreeze()

   /*  Left Freeze */
   ::oLeftView := HBQTableView()
   ::oLeftView:setFont( ::oFont )
   //
   ::oLeftView:setHorizontalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::oLeftView:setVerticalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::oLeftView:setTabKeyNavigation( .t. )
   ::oLeftView:setShowGrid( .t. )
   ::oLeftView:setGridStyle( ::gridStyle )   /* to be based on column definition */
   ::oLeftView:setSelectionMode( QAbstractItemView_SingleSelection )
   ::oLeftView:setSelectionBehavior( iif( ::cursorMode == HBQTBRW_CURSOR_ROW, QAbstractItemView_SelectRows, QAbstractItemView_SelectItems ) )
   ::oLeftView:setFocusPolicy( Qt_NoFocus )
   //
   /*  Veritical Header because of Performance boost */
   ::oLeftVHeaderView := ::oLeftView:verticalHeader()
   ::oLeftVHeaderView:hide()
   /*  Horizontal Header Fine Tuning */
   ::oLeftHeaderView := ::oLeftView:horizontalHeader()
   ::oLeftHeaderView:setHighlightSections( .F. )

   ::oLeftDbfModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 151, t, role, x, y ) } )

   ::oLeftView:setModel( ::oLeftDbfModel )
   //
   //::oLeftView:hide()

   /*  Horizontal Footer */
   ::oLeftFooterView := QHeaderView( Qt_Horizontal )
   //
   ::oLeftFooterView:setHighlightSections( .F. )
   ::oLeftFooterView:setMinimumHeight( 20 )
   ::oLeftFooterView:setMaximumHeight( 20 )
   ::oLeftFooterView:setResizeMode( QHeaderView_Fixed )
   ::oLeftFooterView:setFocusPolicy( Qt_NoFocus )
   //
   ::oLeftFooterModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 152, t, role, x, y ) } )

   ::oLeftFooterView:setModel( ::oLeftFooterModel )
   //
   //::oLeftFooterView:hide()

   RETURN Self


METHOD HbQtBrowse:buildRightFreeze()
   LOCAL oVHdr

   /*  Left Freeze */
   ::oRightView := HBQTableView()
   ::oRightView:setFont( ::oFont )
   //
   ::oRightView:setHorizontalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::oRightView:setVerticalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::oRightView:setTabKeyNavigation( .t. )
   ::oRightView:setShowGrid( .t. )
   ::oRightView:setGridStyle( ::gridStyle )   /* to be based on column definition */
   ::oRightView:setSelectionMode( QAbstractItemView_SingleSelection )
   ::oRightView:setSelectionBehavior( iif( ::cursorMode == HBQTBRW_CURSOR_ROW, QAbstractItemView_SelectRows, QAbstractItemView_SelectItems ) )
   //
   /*  Veritical Header because of Performance boost */
   oVHdr := ::oRightView:verticalHeader()
   oVHdr:hide()
   /*  Horizontal Header Fine Tuning */
   ::oRightHeaderView := ::oRightView:horizontalHeader()
   ::oRightHeaderView:setHighlightSections( .F. )

   ::oRightDbfModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 161, t, role, x, y ) } )

   ::oRightView:setModel( ::oRightDbfModel )

   /*  Horizontal Footer */
   ::oRightFooterView := QHeaderView( Qt_Horizontal )
   //
   ::oRightFooterView:setHighlightSections( .F. )
   ::oRightFooterView:setMinimumHeight( 20 )
   ::oRightFooterView:setMaximumHeight( 20 )
   ::oRightFooterView:setResizeMode( QHeaderView_Fixed )
   ::oRightFooterView:setFocusPolicy( Qt_NoFocus )
   //
   ::oRightFooterModel := HBQAbstractItemModel( {|t,role,x,y| ::supplyInfo( 162, t, role, x, y ) } )

   ::oRightFooterView:setModel( ::oRightFooterModel )

   RETURN Self


METHOD HbQtBrowse:manageMouseDblClick( oMouseEvent )

   IF HB_ISBLOCK( ::bNavigationBlock )
      SWITCH oMouseEvent:button()
      CASE Qt_LeftButton
         Eval( ::bNavigationBlock, K_LDBLCLK, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_RightButton
         Eval( ::bNavigationBlock, K_RDBLCLK, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_MidButton
         Eval( ::bNavigationBlock, K_MDBLCLK, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self


METHOD HbQtBrowse:manageMouseWheel( oWheelEvent )

   IF oWheelEvent:orientation() == Qt_Vertical
      IF oWheelEvent:delta() > 0
         ::skipRows( -1 )
      ELSE
         ::skipRows( 1 )
      ENDIF
   ELSE
      IF oWheelEvent:delta() > 0
         ::skipCols( 1 )
      ELSE
         ::skipCols( -1 )
      ENDIF
   ENDIF

   IF oWheelEvent:orientation() == Qt_Vertical .AND. HB_ISBLOCK( ::bNavigationBlock )
      IF oWheelEvent:delta() > 0
         Eval( ::bNavigationBlock, K_MWBACKWARD, { oWheelEvent:globalX(), oWheelEvent:globalY() }, Self )
      ELSE
         Eval( ::bNavigationBlock, K_MWFORWARD , { oWheelEvent:globalX(), oWheelEvent:globalY() }, Self )
      ENDIF
   ENDIF

   RETURN Self

METHOD HbQtBrowse:manageMousePress( oMouseEvent )

   ::oModelIndex := ::oTableView:indexAt( oMouseEvent:pos() )
   IF ::oModelIndex:isValid()
      ::skipRows( ( ::oModelIndex:row() + 1 ) - ::rowPos )
      ::skipCols( ( ::oModelIndex:column() + 1 ) - ::colPos )
   ENDIF

   IF HB_ISBLOCK( ::bNavigationBlock )
      SWITCH oMouseEvent:button()
      CASE Qt_LeftButton
         Eval( ::bNavigationBlock, K_LBUTTONDOWN, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_RightButton
         Eval( ::bNavigationBlock, K_RBUTTONDOWN, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_MidButton
         Eval( ::bNavigationBlock, K_MBUTTONDOWN, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self


METHOD HbQtBrowse:manageMouseRelease( oMouseEvent )

   IF HB_ISBLOCK( ::bNavigationBlock )
      SWITCH oMouseEvent:button()
      CASE Qt_LeftButton
         Eval( ::bNavigationBlock, K_LBUTTONUP, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_RightButton
         Eval( ::bNavigationBlock, K_MBUTTONUP, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      CASE Qt_MidButton
         Eval( ::bNavigationBlock, K_MBUTTONUP, { oMouseEvent:globalX(), oMouseEvent:globalY() }, Self )
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self


METHOD HbQtBrowse:manageKeyPress( oEvent )

   LOCAL lHandelled := .F.
   LOCAL nKey

   nKey := hbqt_qtEventToHbEvent( oEvent )

   IF HB_ISBLOCK( SetKey( nKey ) )
      Eval( SetKey( nKey ) )
      RETURN .F.
   ENDIF

   IF HB_ISBLOCK( ::bNavigationBlock )
      lHandelled := Eval( ::bNavigationBlock, nKey, NIL, Self )
      IF ! HB_ISLOGICAL( lHandelled )
         lHandelled := .F.
      ENDIF
   ENDIF

   IF ! lHandelled
      ::applyKey( nKey )
   ENDIF

   RETURN .T.   /* Stop Propegation to parent */


METHOD HbQtBrowse:execSlot( nEvent, p1, p2, p3 )

   SWITCH nEvent
   CASE __ev_tableViewBlock_main__
      SWITCH p1
      CASE QEvent_MouseButtonPress
         ::manageMousePress( p2 )
         EXIT
      CASE QEvent_MouseButtonRelease
         ::manageMouseRelease( p2 )
         EXIT
      CASE QEvent_MouseButtonDblClick
         ::manageMouseDblClick( p2 )
         EXIT
      CASE QEvent_Wheel
         ::manageMouseWheel( p2 )
         EXIT
      CASE HBQT_HBQTABLEVIEW_scrollContentsBy
         ::manageScrollContents( p2, p3 )
         EXIT
      ENDSWITCH
      EXIT
   CASE __ev_mousepress_on_frozen__
      ::oTableView:setFocus( 0 )
      EXIT
   CASE __ev_horzscroll_via_qt__
      ::manageScrollContents( p1, p2 )
      EXIT
   CASE __ev_vertscroll_via_user__
      SWITCH p1
      CASE QAbstractSlider_SliderNoAction
         RETURN NIL
      CASE QAbstractSlider_SliderSingleStepAdd
         ::down()
         EXIT
      CASE QAbstractSlider_SliderSingleStepSub
         ::up()
         EXIT
      CASE QAbstractSlider_SliderPageStepAdd
         ::pageDown()
         EXIT
      CASE QAbstractSlider_SliderPageStepSub
         ::pageUp()
         EXIT
      CASE QAbstractSlider_SliderToMinimum
         ::goTop()
         EXIT
      CASE QAbstractSlider_SliderToMaximum
         ::goBottom()
         EXIT
      CASE QAbstractSlider_SliderMove
         ::updatePosition()
         EXIT
      ENDSWITCH
      ::oTableView:setFocus( 0 )
      EXIT
   CASE __ev_vertscroll_sliderreleased__
      ::updatePosition()
      ::oTableView:setFocus()
      EXIT
   CASE __ev_horzscroll_slidermoved__
      ::skipCols( ( ::oHScrollBar:value() + 1 ) - ::colPos )
      ::oTableView:setFocus()
      EXIT
   CASE __ev_horzscroll_sliderreleased__
      ::skipCols( ( ::oHScrollBar:value() + 1 ) - ::colPos )
      ::oTableView:setFocus()
      EXIT
   CASE __ev_headersec_resized__
      ::oFooterView:resizeSection( p1, p3 )
      EXIT
   CASE __ev_footersec_resized__
      ::oHeaderView:resizeSection( p1, p3 )
      EXIT

   ENDSWITCH

   RETURN .F.


METHOD HbQtBrowse:manageFrameResized()
   LOCAL nRowPos, nColPos, nOff

   ::oHeaderView:resizeSection( 0, ::oHeaderView:sectionSize( 0 )+1 )
   ::oHeaderView:resizeSection( 0, ::oHeaderView:sectionSize( 0 )-1 )

   nRowPos := ::rowPos()
   nColPos := ::colPos()
   //
   ::nConfigure := 9
   ::doConfigure()
   //
   ::colPos := nColPos
   IF nRowPos > ::rowCount()
      nOff := nRowPos - ::rowCount()
      ::rowPos := ::rowCount()
   ELSE
      nOff := 0
   ENDIF
   ::refreshAll()
   ::forceStable()
   ::setCurrentIndex( nRowPos > ::rowCount() )
   IF nOff > 0
      ::skipRows( nOff )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:manageCommitData( qWidget )
   LOCAL cTxt  := qWidget:property( "text" ):toString()
   LOCAL oCol  := ::columns[ ::colPos ]
   LOCAL cTyp  := valtype( eval( oCol:block ) )

   HB_TRACE( HB_TR_DEBUG, cTxt, cTyp )
   DO CASE
   CASE cTyp == "C"
      Eval( oCol:block, cTxt )
   CASE cTyp == "N"
      Eval( oCol:block, val( cTxt ) )
   CASE cTyp == "D"
      Eval( oCol:block, ctod( cTxt ) )
   CASE cTyp == "L"
      Eval( oCol:block, cTxt $ "Yy" )
   ENDCASE

   ::refreshCurrent()
   ::oTableView:setFocus()

   RETURN Self


METHOD HbQtBrowse:manageEditorClosed( pWidget, nHint )

   pWidget:close()

   SWITCH nHint
   CASE QAbstractItemDelegate_NoHint                 /* 0  RETURN is presses    */

   CASE QAbstractItemDelegate_EditNextItem           /* 1  TAB is pressed       */
      ::skipCols( 1 )

   CASE QAbstractItemDelegate_EditPreviousItem       /* 2  SHIFT_TAB is pressed */
      ::skipCols( -1 )

   CASE QAbstractItemDelegate_SubmitModelCache       /* 3  ENTER is pressed     */
      #if 0
      IF ::colPos < ::colCount
         ::skipCols( 1 )
         ::editCell()
      ENDIF
      #endif
   CASE QAbstractItemDelegate_RevertModelCache       /* 4  ESC is pressed       */

   ENDSWITCH

   RETURN Self


METHOD HbQtBrowse:manageScrollContents( nX, nY )

   HB_SYMBOL_UNUSED( nY )

   IF nX != 0
      ::setHorzOffset()
   ENDIF

   RETURN Self


METHOD HbQtBrowse:openPersistentEditor()

   ::oTableView:openPersistentEditor( ::oDbfModel:index( ::rowPos - 1, ::colPos - 1 ) )

   RETURN Self


METHOD HbQtBrowse:supplyInfo( nMode, nCall, nRole, nX, nY )

   IF nCall == HBQT_QAIM_headerData .AND. nX == Qt_Vertical
      RETURN NIL
   ENDIF

   IF nCall == HBQT_QAIM_flags
      RETURN Qt_ItemIsEnabled + Qt_ItemIsSelectable + Qt_ItemIsEditable
   ENDIF

   DO CASE
   CASE nMode == 141       /* Main View Header|Data */
      IF nCall == HBQT_QAIM_columnCount
         IF ::colCount > 0
            ::forceStable()
            ::setHorzScrollBarRange( .t. )
         ENDIF
         RETURN ::colCount
      ELSEIF nCall == HBQT_QAIM_rowCount
         IF ::colCount > 0
            ::forceStable()
            ::setVertScrollBarRange( .f. )
         ENDIF
         RETURN ::rowCount
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall, nRole, 0, nY+1, nX+1 )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall, nRole, 0, 0, nY+1 )
      ENDIF
      RETURN nil

   CASE nMode == 142       /* Main View Footer */
      IF nCall == HBQT_QAIM_columnCount
         IF ::colCount > 0
            ::forceStable()
         ENDIF
         RETURN ::colCount
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall, nRole, 1, nY+1, nX+1 )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall, nRole, 1, 0, nY+1 )
      ENDIF
      RETURN nil

   CASE nMode == 151       /* Left Frozen Header|Data */
      IF nCall == HBQT_QAIM_columnCount
         IF ::nLeftFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::nLeftFrozen
      ELSEIF nCall == HBQT_QAIM_rowCount
         IF ::nLeftFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::rowCount
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall,nRole, 0, nY+1, ::aLeftFrozen[ nX+1 ] )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall,nRole, 0, 0, ::aLeftFrozen[ nY+1 ] )
      ENDIF
      RETURN NIL

   CASE nMode == 152       /* Left Frozen Footer */
      IF nCall == HBQT_QAIM_columnCount
         IF ::nLeftFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::nLeftFrozen
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall,nRole, 1, nY+1, ::aLeftFrozen[ nX+1 ] )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall,nRole, 1, 0, ::aLeftFrozen[ nY+1 ] )
      ENDIF

   CASE nMode == 161       /* Right Frozen Header|Data */
      IF nCall == HBQT_QAIM_columnCount
         IF ::nRightFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::nRightFrozen
      ELSEIF nCall == HBQT_QAIM_rowCount
         IF ::nRightFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::rowCount
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall,nRole, 0, nY+1, ::aRightFrozen[ nX+1 ] )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall,nRole, 0, 0, ::aRightFrozen[ nY+1 ] )
      ENDIF

   CASE nMode == 162       /* Right Frozen Footer */
      IF nCall == HBQT_QAIM_columnCount
         IF ::nRightFrozen > 0
            ::forceStable()
         ENDIF
         RETURN ::nRightFrozen
      ELSEIF nCall == HBQT_QAIM_data
         RETURN ::fetchColumnInfo( nCall,nRole, 1, nY+1, ::aRightFrozen[ nX+1 ] )
      ELSEIF nCall == HBQT_QAIM_headerData
         RETURN ::fetchColumnInfo( nCall,nRole, 1, 0, ::aRightFrozen[ nY+1 ] )
      ENDIF

   ENDCASE

   RETURN NIL


METHOD HbQtBrowse:fetchColumnInfo( nCall, nRole, nArea, nRow, nCol )
   LOCAL oCol := ::columns[ nCol ]

   SWITCH nCall
   CASE HBQT_QAIM_data

      SWITCH ( nRole )
      CASE Qt_ForegroundRole
          RETURN ::compatColor( __hbqtHbColorToQtValue( ::colorValue( ::cellColor( nRow, nCol )[ 1 ] ), Qt_ForegroundRole ) )

      CASE Qt_BackgroundRole
         RETURN ::compatColor( __hbqtHbColorToQtValue( ::colorValue( ::cellColor( nRow, nCol )[ 1 ] ), Qt_BackgroundRole ) )

      CASE Qt_TextAlignmentRole
         RETURN oCol:dAlignment

      CASE Qt_SizeHintRole
         RETURN ::oDefaultCellSize

      CASE Qt_DecorationRole
         IF oCol:type == HBQTCOL_TYPE_FILEICON
            RETURN ::compatIcon( ::cellValue( nRow, nCol ) )
         ELSE
            RETURN NIL
         ENDIF
      CASE Qt_EditRole
      CASE Qt_DisplayRole
         IF oCol:type == HBQTCOL_TYPE_FILEICON
            RETURN NIL
         ELSE
            IF nCall == Qt_EditRole    /* Never Reached */
               SWITCH oCol:valtype
               CASE "C"
                  RETURN Trim( Eval( oCol:block ) )
               CASE "N"
                  RETURN Val( Eval( oCol:block ) )
               CASE "D"
                  RETURN DToC( Eval( oCol:block ) )
               CASE "L"
                  RETURN iif( Eval( oCol:block ), "T", "F" )
               ENDSWITCH
               RETURN ""
            ELSE
               RETURN ::cellValue( nRow, nCol )
            ENDIF
         ENDIF
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_headerData
      IF nArea == 0                    /* Header Area */
         SWITCH nRole
         CASE Qt_SizeHintRole
            RETURN ::oDefaultCellSize //oCol:hHeight
         CASE Qt_DisplayRole
            RETURN oCol:heading
         CASE Qt_TextAlignmentRole
            RETURN oCol:hAlignment
         CASE Qt_ForegroundRole
            RETURN ::compatColor( oCol:hFgColor )
         CASE Qt_BackgroundRole
            RETURN ::compatColor( oCol:hBgColor )
         ENDSWITCH
      ELSE                             /* Footer Area */
         SWITCH nRole
         CASE Qt_SizeHintRole
            RETURN ::oDefaultCellSize //oCol:fHeight
         CASE Qt_DisplayRole
            RETURN oCol:footing
         CASE Qt_TextAlignmentRole
            RETURN oCol:fAlignment
         CASE Qt_ForegroundRole
            RETURN ::compatColor( oCol:fFgColor )
         CASE Qt_BackgroundRole
            RETURN ::compatColor( oCol:fBgColor )
         ENDSWITCH
      ENDIF
      RETURN NIL
   ENDSWITCH

   RETURN NIL


METHOD HbQtBrowse:compatColor( nColor )
   IF ! hb_hHasKey( ::hColors, nColor )
      ::hColors[ nColor ] := QColor( nColor )
   ENDIF
   RETURN ::hColors[ nColor ]

METHOD HbQtBrowse:compatBrush( nColor )
   IF ! hb_hHasKey( ::hBrushes, nColor )
      ::hBrushes[ nColor ] := QBrush( nColor )
   ENDIF
   RETURN ::hBrushes[ nColor ]

METHOD HbQtBrowse:compatIcon( cIcon )
   IF ! hb_hHasKey( ::hIcons, cIcon )
      ::hIcons[ cIcon ] := QIcon( QPixmap( Trim( cIcon ) ) )
   ENDIF
   RETURN ::hIcons[ cIcon ]


METHOD HbQtBrowse:setVertScrollBarRange( lPageStep )
   LOCAL nMin, nMax

   IF HB_ISBLOCK( ::bFirstPosBlock ) .AND. HB_ISBLOCK( ::bLastPosBlock )
      hb_default( @lPageStep, .F. )

      IF HB_ISNUMERIC( nMin := eval( ::bFirstPosBlock  ) ) .AND. HB_ISNUMERIC( nMax := eval( ::bLastPosBlock ) )
         ::oVScrollBar:setMinimum( nMin - 1 )
         ::oVScrollBar:setMaximum( nMax - 1 )
         ::oVScrollBar:setSingleStep( 1 )
         //
         IF lPageStep
            ::oVScrollBar:setPageStep( ::rowCount() )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self


METHOD HbQtBrowse:setHorzScrollBarRange( lPageStep )

   hb_default( @lPageStep, .F. )

   IF HB_ISOBJECT( ::oHScrollBar )
      WITH OBJECT ::oHScrollBar
         :setMinimum( 0 )
         :setMaximum( ::colCount - 1 )
         :setSingleStep( 1 )
         :setPageStep( 1 )
      ENDWITH
   ENDIF

   RETURN Self


METHOD HbQtBrowse:updatePosition()

   IF HB_ISBLOCK( ::goPosBlock )
      eval( ::goPosBlock, ::oVScrollBar:value() + 1 )
      ::refreshAll()
      ::forceStable()
      ::setCurrentIndex()
   ENDIF

   RETURN Self


METHOD HbQtBrowse:updateVertScrollBar()

   IF HB_ISBLOCK( ::posBlock )
      ::oVScrollBar:setValue( eval( ::posBlock ) - 1 )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:setHorzOffset()

   IF ::colPos == ::colCount
      ::oHeaderView:setOffsetToLastSection()
      ::oFooterView:setOffsetToLastSection()
   ELSE
      ::oHeaderView:setOffsetToSectionPosition( ::colPos - 1 )
      ::oFooterView:setOffsetToSectionPosition( ::colPos - 1 )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:setCurrentIndex( lReset )

   hb_default( @lReset, .T. )

   IF lReset
      ::oDbfModel:reset()                         /* Important */
      //
      IF HB_ISOBJECT( ::oLeftDbfModel )
         ::oLeftDbfModel:reset()
      ENDIF
      IF HB_ISOBJECT( ::oRightDbfModel )
         ::oRightDbfModel:reset()
      ENDIF
   ENDIF
   ::oTableView:setCurrentIndex( ::oDbfModel:index( ::rowPos - 1, ::colPos - 1 ) )

   RETURN Self


METHOD HbQtBrowse:horizontalScrollbar( lYes )

   IF HB_ISLOGICAL( lYes )
      ::lHScroll := lYes
      ::setUnstable()
      ::configure( 128 )
   ENDIF

   RETURN ::lHScroll


METHOD HbQtBrowse:verticalScrollbar( lYes )

   IF HB_ISLOGICAL( lYes )
      ::lVScroll := lYes
      ::setUnstable()
      ::configure( 128 )
   ENDIF

   RETURN ::lHScroll


METHOD HbQtBrowse:sizeCols( lYes )

   IF HB_ISLOGICAL( lYes )
      ::lSizeCols := lYes
      ::setUnstable()
      ::configure( 128 )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:cursorMode( nMode )

   IF HB_ISNUMERIC( nMode )
      ::nCursorMode := nMode
      ::setUnstable()
      ::configure( 128 )
   ENDIF

   RETURN ::nCursorMode


METHOD HbQtBrowse:setRightFrozen( aColFrozens )
   LOCAL aFrozen := aclone( ::aRightFrozen )

   IF HB_ISARRAY( aColFrozens )
      ::aRightFrozen := aColFrozens
      ::nRightFrozen := len( ::aRightFrozen )
      ::setUnstable()
      ::configure( 128 )
      ::forceStable()
   ENDIF

   RETURN aFrozen


METHOD HbQtBrowse:setLeftFrozen( aColFrozens )
   LOCAL aFrozen := aclone( ::aLeftFrozen )

   IF HB_ISARRAY( aColFrozens )
      ::aLeftFrozen := aColFrozens
      ::nLeftFrozen := len( ::aLeftFrozen )
      ::setUnstable()
      ::configure( 128 )
      ::forceStable()
   ENDIF

   RETURN aFrozen


METHOD HbQtBrowse:setCellHeight( nCellHeight )
   LOCAL i

   FOR i := 1 TO ::nRowsInView
      ::oTableView : setRowHeight( i-1, nCellHeight )
      IF !empty( ::oLeftView )
         ::oLeftView  : setRowHeight( i-1, nCellHeight )
      ENDIF
      IF !empty( ::oRightView )
         ::oRightView : setRowHeight( i-1, nCellHeight )
      ENDIF
   NEXT
   RETURN Self


METHOD HbQtBrowse:cellValue( nRow, nCol )

   IF nRow >= 1 .AND. nRow <= ::rowCount .AND. ;
      nCol >= 1 .AND. nCol <= ::colCount .AND. ;
      ::aCellStatus[ nRow ]

      RETURN ::aCellValues[ nRow, nCol ]
   ENDIF

   RETURN NIL


METHOD HbQtBrowse:cellValueA( nRow, nCol )

   IF nRow >= 1 .AND. nRow <= ::rowCount .AND. ;
      nCol >= 1 .AND. nCol <= ::colCount .AND. ;
      ::aCellStatus[ nRow ]

      RETURN ::aCellValuesA[ nRow, nCol ]
   ENDIF

   RETURN NIL


METHOD HbQtBrowse:navigationBlock( bBlock )
   IF bBlock != NIL
      ::bNavigationBlock := __eInstVar53( Self, "NAVIGATIONBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bNavigationBlock

METHOD HbQtBrowse:firstPosBlock( bBlock )
   IF bBlock != NIL
      ::bFirstPosBlock := __eInstVar53( Self, "FIRSTPOSBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bFirstPosBlock

METHOD HbQtBrowse:lastPosBlock( bBlock )
   IF bBlock != NIL
      ::bLastPosBlock := __eInstVar53( Self, "LASTPOSBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bLastPosBlock

METHOD HbQtBrowse:phyPosBlock( bBlock )
   IF bBlock != NIL
      ::bPhyPosBlock := __eInstVar53( Self, "PHYPOSBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bPhyPosBlock

METHOD HbQtBrowse:posBlock( bBlock )
   IF bBlock != NIL
      ::bPosBlock := __eInstVar53( Self, "POSBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bPosBlock

METHOD HbQtBrowse:goPosBlock( bBlock )
   IF bBlock != NIL
      ::bGoPosBlock := __eInstVar53( Self, "GOPOSBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bGoPosBlock

METHOD HbQtBrowse:hitBottomBlock( bBlock )
   IF bBlock != NIL
      ::bHitBottomBlock := __eInstVar53( Self, "HITBOTTOMBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bHitBottomBlock

METHOD HbQtBrowse:hitTopBlock( bBlock )
   IF bBlock != NIL
      ::bHitTopBlock := __eInstVar53( Self, "HITTOPBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bHitTopBlock

METHOD HbQtBrowse:stableBlock( bBlock )
   IF bBlock != NIL
      ::bStableBlock := __eInstVar53( Self, "STABLEBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bStableBlock

METHOD HbQtBrowse:editBlock( bBlock )
   IF bBlock != NIL
      ::bEditBlock := __eInstVar53( Self, "EDITBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bEditBlock

METHOD HbQtBrowse:searchBlock( bBlock )
   IF bBlock != NIL
      ::bSearchBlock := __eInstVar53( Self, "SEARCHBLOCK", bBlock, "B", 1001 )
   ENDIF
   RETURN ::bSearchBlock

METHOD HbQtBrowse:skipCols( nCols )
   LOCAL i

   IF nCols < 0
      FOR i := 1 TO abs( nCols )
         ::left()
      NEXT
   ELSEIF nCols > 0
      FOR i := 1 TO nCols
         ::right()
      NEXT
   ENDIF

   RETURN Self


METHOD HbQtBrowse:skipRows( nRows )
   LOCAL i

   IF nRows < 0
      FOR i := 1 TO abs( nRows )
         ::up()
      NEXT
   ELSEIF nRows > 0
      FOR i := 1 TO nRows
         ::down()
      NEXT
   ENDIF

   RETURN Self


/* get number of frozen columns */
METHOD HbQtBrowse:getFrozen()

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nLeftFrozen


METHOD HbQtBrowse:freeze( nColumns )  /* Overloaded */
   LOCAL i, n := ::nLeftFrozen

   nColumns := Max( 0, Int( nColumns ) )

   IF ! Empty( nColumns )
      IF nColumns + ::nRightFrozen < ::colCount
         ::aLeftFrozen := {}
         FOR i := 1 TO nColumns
            AAdd( ::aLeftFrozen, i )
         NEXT
      ENDIF
   ELSE
      ::aLeftFrozen := {}
   ENDIF
   ::nLeftFrozen := Len( ::aLeftFrozen )
   ::nFrozen := Len( ::aLeftFrozen )
   ::setUnstable()
   ::configure( 128 )
   ::forceStable()

   IF n < ::nFrozen
      ::Right()
      IF ::colPos < ::nFrozen
         ::Right()
      ENDIF
   ENDIF
   ::down()
   IF ! ::hitBottom()
      ::up()
   ENDIF

   RETURN ::nLeftFrozen


METHOD HbQtBrowse:getRFrozen()         /* Not a TBrowse METHOD  */

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nRightFrozen


METHOD HbQtBrowse:rFreeze( nColumns )  /* Not a TBrowse METHOD  */
   LOCAL i, n := ::nRightFrozen

   ::nRightFrozen := Max( 0, Int( nColumns ) )
   IF ! Empty( ::nRightFrozen )
      IF ::nFrozen + ::nRightFrozen < ::colCount  /* At least 1 column must remain IN table view */
         ::aRightFrozen := {}
         FOR i := ::nRightFrozen TO 1 STEP -1
            AAdd( ::aRightFrozen, ::colCount - i + 1 )
         NEXT
      ENDIF
   ELSE
      ::aRightFrozen := {}
   ENDIF
   ::nRightFrozen := Len( ::aRightFrozen )
   ::setUnstable()
   ::configure( 128 )
   ::forceStable()
   IF n < ::nRightFrozen
      IF ::colPos > ::colCount - ::nRightFrozen
         ::Left()
      ENDIF
   ENDIF
   ::down()
   IF ! ::hitBottom()
      ::up()
   ENDIF

   RETURN ::nRightFrozen


METHOD HbQtBrowse:rowCount()  /* Overloaded */
   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF
   RETURN ::nRowsInView


METHOD HbQtBrowse:refreshAll()

   ::TBrowse:refreshAll()

   ::forceStable()
   ::setCurrentIndex( .T. )

   RETURN Self


METHOD HbQtBrowse:refreshCurrent()

   ::TBrowse:refreshCurrent()

   ::forceStable()
   ::setCurrentIndex( .T. )

   RETURN Self


METHOD HbQtBrowse:up()
   LOCAL lReset := ::rowPos == 1

   ::TBrowse:up()

   ::forceStable()
   ::setCurrentIndex( lReset )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:down()
   LOCAL lReset := ::rowPos >= ::rowCount

   ::TBrowse:down()

   ::forceStable()
   ::setCurrentIndex( lReset )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:pageUp()

   ::TBrowse:pageUp()

   ::forceStable()
   ::setCurrentIndex( .T. )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:pageDown()

   ::TBrowse:pageDown()

   ::forceStable()
   ::setCurrentIndex( .t. )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:goTop()

   ::TBrowse:goTop()

   ::forceStable()
   ::setCurrentIndex( .T. )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:goBottom()

   ::TBrowse:goBottom()

   ::forceStable()
   ::setCurrentIndex( .T. )
   ::updateVertScrollBar()

   RETURN Self


METHOD HbQtBrowse:left()
   LOCAL n, nCol
   LOCAL lUpdate := .f.

   nCol := ::colPos

   IF nCol > 1
      DO WHILE --nCol >= 1
         IF ! ISFROZEN( nCol )
            lUpdate := .t.
            EXIT
         ENDIF
      ENDDO
   ENDIF
   IF lUpdate
      ::colPos := nCol

      n  := ::oHeaderView:sectionViewportPosition( ::colPos-1 )
      IF n < 0
         ::oHeaderView:setOffset( ::oHeaderView:offSet() + n )
         ::oFooterView:setOffset( ::oFooterView:offSet() + n )
         ::setCurrentIndex( .T. )
      ELSE
         ::setCurrentIndex( .F. )
      ENDIF

      ::oHScrollBar:setValue( ::colPos - 1 )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:right()
   LOCAL n, n1, n2, nLnWidth, nCol
   LOCAL lUpdate := .F.

   IF ::colPos < ::colCount
      nCol := ::colPos

      DO WHILE ++nCol <= ::colCount
         IF !( ISFROZEN( nCol ) )
            lUpdate := .T.
            EXIT
         ENDIF
      ENDDO

      IF lUpdate
         ::colPos := nCol

         n  := ::oHeaderView:sectionViewportPosition( ::colPos - 1 )
         n1 := ::oHeaderView:sectionSize( ::colPos-1 )
         n2 := ::oViewport:width()

         IF n + n1 > n2
            nLnWidth := ::oTableView:lineWidth()
            IF n1 > n2
               ::oHeaderView:setOffset( ::oHeaderView:sectionPosition( ::colPos - 1 ) )
               ::oFooterView:setOffset( ::oHeaderView:sectionPosition( ::colPos - 1 ) )

            ELSE
               ::oHeaderView:setOffset( ::oHeaderView:offSet()+(n1-(n2-n)+1) - nLnWidth )
               ::oFooterView:setOffset( ::oFooterView:offSet()+(n1-(n2-n)+1) - nLnWidth )

            ENDIF
            ::setCurrentIndex( .T. )
         ELSE
            ::setCurrentIndex( .F. )
         ENDIF

         ::oHScrollBar:setValue( ::colPos - 1 )
      ENDIF
   ENDIF

   RETURN Self


METHOD HbQtBrowse:firstCol()
   LOCAL n, nCol
   LOCAL lUpdate := .f.

   nCol := 0

   DO WHILE ++nCol <= ::colCount
      IF !ISFROZEN( nCol )
         lUpdate := .t.
         EXIT
      ENDIF
   ENDDO

   IF lUpdate
      ::setUnstable()
      ::colPos := nCol

      n  := ::oHeaderView:sectionViewportPosition( ::colPos-1 )
      IF n < 0
         ::oHeaderView:setOffset( ::oHeaderView:offSet() + n )
         ::oFooterView:setOffset( ::oFooterView:offSet() + n )
         ::setCurrentIndex( .t. )
      ELSE
         ::setCurrentIndex( .f. )
      ENDIF
      ::oHScrollBar:setValue( 0 )
   ENDIF
   RETURN Self


METHOD HbQtBrowse:lastCol()
   LOCAL n, n1, n2, nCol
   LOCAL lUpdate := .f.

   nCol := ::colCount + 1

   DO WHILE --nCol >= 1
      IF !ISFROZEN( nCol )
         lUpdate := .t.
         EXIT
      ENDIF
   ENDDO

   IF lUpdate
      ::setUnstable()
      ::colPos := nCol

      n  := ::oHeaderView:sectionViewportPosition( ::colPos-1 )
      n1 := ::oHeaderView:sectionSize( ::colPos-1 )
      n2 := ::oViewport:width()
      IF n + n1 > n2
         IF n1 > n2
            ::oHeaderView:setOffset( ::oHeaderView:sectionPosition( ::colPos - 1 ) )
            ::oFooterView:setOffset( ::oHeaderView:sectionPosition( ::colPos - 1 ) )
         ELSE
            ::oHeaderView:setOffset( ::oHeaderView:offSet()+(n1-(n2-n)+1) - ::oTableView:lineWidth() )
            ::oFooterView:setOffset( ::oFooterView:offSet()+(n1-(n2-n)+1) - ::oTableView:lineWidth() )
         ENDIF
         ::setCurrentIndex( .t. )
      ELSE
         ::setCurrentIndex( .f. )
      ENDIF
      ::oHScrollBar:setValue( ::colCount - 1 )
   ENDIF

   RETURN Self


METHOD HbQtBrowse:home()

   ::colPos := max( 1, ::oHeaderView:visualIndexAt( 1 ) + 1 )
   ::setCurrentIndex( .t. )

   RETURN Self


METHOD HbQtBrowse:end()

   ::nRightVisible := ::oHeaderView:visualIndexAt( ::oViewport:width()-2 ) + 1
   IF ::nRightVisible == 0
      ::nRightVisible := ::colCount
   ENDIF
   ::colPos := ::nRightVisible
   ::setCurrentIndex( .t. )

   RETURN Self


METHOD HbQtBrowse:panHome()
   RETURN ::firstCol()


METHOD HbQtBrowse:panEnd()
   RETURN ::lastCol()


METHOD HbQtBrowse:panLeft()
   LOCAL nLeftVisible := ::oHeaderView:visualIndexAt( 1 )+1

   IF nLeftVisible > 1
      ::oHeaderView:setOffSet( ::oHeaderView:sectionPosition( nLeftVisible - 2 ) )
      ::oFooterView:setOffSet( ::oFooterView:sectionPosition( nLeftVisible - 2 ) )
   ENDIF
   ::oDbfModel:reset()

   RETURN Self


METHOD HbQtBrowse:panRight()
   ::TBRowse:panRight()
   RETURN Self


METHOD HbQtBrowse:manageColumnMoved( nLogicalIndex, nOldVisualIndex, nNewVisualIndex )
   LOCAL save_col := ::getColumn( nOldVisualIndex + 1 )

   HB_SYMBOL_UNUSED( nLogicalIndex )

   ::delColumn( nOldVisualIndex + 1 )
   IF nOldVisualIndex > nNewVisualIndex    /* Moved to left */
      ::insColumn( nNewVisualIndex + 1, save_col )
   ELSE
      ::insColumn( nNewVisualIndex, save_col )
   ENDIF
   ::configure()
   ::refreshAll()

   HB_TRACE( HB_TR_ALWAYS, nOldVisualIndex, nNewVisualIndex )
   RETURN Self

METHOD HbQtBrowse:moveLeft()
   LOCAL save_col, col_to_move := ::colPos

   IF col_to_move > 1
      save_col := ::getColumn( col_to_move )
      ::setColumn( col_to_move, ::getcolumn( col_to_move - 1 ) )
      ::setColumn( col_to_move - 1, save_col )
      QApplication():sendEvent( ::oTableView, QKeyEvent( QEvent_KeyPress, Qt_Key_Left, Qt_NoModifier ) )
   ENDIF

   RETURN col_to_move > 1


METHOD HbQtBrowse:moveRight()
   LOCAL save_col, col_to_move := ::colPos

   IF col_to_move < ::colCount
      save_col := ::getColumn( col_to_move )
      ::setColumn( col_to_move, ::getColumn( col_to_move + 1 ) )
      ::setColumn( col_to_move + 1, save_col )
      QApplication():sendEvent( ::oTableView, QKeyEvent( QEvent_KeyPress, Qt_Key_Right, Qt_NoModifier ) )
   ENDIF

   RETURN col_to_move < ::colCount


METHOD HbQtBrowse:moveHome()
   LOCAL save_col, col_to_move := ::colPos

   IF col_to_move > 1
      save_col := ::getColumn( col_to_move )
      ::delColumn( col_to_move )
      ::insColumn( 1, save_col )
      QApplication():sendEvent( ::oTableView, QKeyEvent( QEvent_KeyPress, Qt_Key_Home, Qt_ControlModifier ) )
   ENDIF

   RETURN col_to_move > 1


METHOD HbQtBrowse:moveEnd()
   LOCAL save_col, col_to_move := ::colPos

   IF col_to_move < ::colCount
      save_col := ::getColumn( col_to_move )
      ::addColumn( save_col )
      ::delColumn( col_to_move )
      QApplication():sendEvent( ::oTableView, QKeyEvent( QEvent_KeyPress, Qt_Key_End, Qt_ControlModifier ) )
   ENDIF

   RETURN col_to_move < ::colCount


METHOD HbQtBrowse:getEditable()
   RETURN ::lEditable

METHOD HbQtBrowse:setEditable( lEdit )
   IF HB_ISLOGICAL( lEdit )
      ::lEditable := lEdit
      ::oActEdit:setEnabled( ::lEditable )
   ENDIF
   RETURN ::lEditable


METHOD HbQtBrowse:edit( cTitle, lSaveOnLastGet, lDownAfterSave )
   LOCAL oDlg, oVLayout, oScrollArea, oPos, oCol
   LOCAL oBtn1, oBtn2, oBtn3, oBtn4, oBtn5, oToolbar
   LOCAL oEditor, oFLayout, aFrame
   LOCAL k1, k2, k3, k4, k5
   LOCAL GetList := {}, SayList := {}

   hb_default( @cTitle, "Modify Row Data" )
   hb_default( @lSaveOnLastGet, .T. )
   hb_default( @lDownAfterSave, .T. )

   ::lSaveOnLastGet := lSaveOnLastGet
   ::lDownAfterSave := lDownAfterSave

   oDlg := QDialog( ::oWidget )
   oDlg:setWindowTitle( cTitle )

   oEditor := QWidget()
   oFLayout := QFormLayout()
   oEditor:setLayout( oFLayout )

   oScrollArea := QScrollArea( oDlg )
   oScrollArea:setWidget( oEditor )

   oVLayout := QVBoxLayout( oDlg )
   oVLayout:setContentsMargins( 0,0,0,0 )
   oVLayout:setSpacing( 0 )

   oVLayout:addWidget( oScrollArea )

   WITH OBJECT oBtn1 := QToolButton( oDlg )
      :setText( "Save" )
      :setIcon( QIcon( __hbqtImage( "save3" ) ) )
      :setAutoRaise( .T. )
      :setTooltip( "Save editied record" )
      :connect( "clicked()", {||  ::saveRow() } )
   ENDWITH
   WITH OBJECT oBtn2 := QToolButton( oDlg )
      :setText( "Down" )
      :setIcon( QIcon( __hbqtImage( "go-down" ) ) )
      :setAutoRaise( .T. )
      :setTooltip( "Go down one record" )
      :connect( "clicked()", {||  ::down()    , ::loadRow(), ::populateGets() } )
   ENDWITH
   WITH OBJECT oBtn3 := QToolButton( oDlg )
      :setText( "Up" )
      :setIcon( QIcon( __hbqtImage( "go-up" ) ) )
      :setAutoRaise( .T. )
      :setTooltip( "Go up one record" )
      :connect( "clicked()", {||  ::up()      , ::loadRow(), ::populateGets() } )
   ENDWITH
   WITH OBJECT oBtn4 := QToolButton( oDlg )
      :setText( "Bttm" )
      :setIcon( QIcon( __hbqtImage( "go-bottom" ) ) )
      :setAutoRaise( .T. )
      :setTooltip( "Go bottom of browser" )
      :connect( "clicked()", {||  ::goBottom(), ::loadRow(), ::populateGets() } )
   ENDWITH
   WITH OBJECT oBtn5 := QToolButton( oDlg )
      :setText( "Top" )
      :setIcon( QIcon( __hbqtImage( "go-top" ) ) )
      :setAutoRaise( .T. )
      :setTooltip( "Go top of browser" )
      :connect( "clicked()", {||  ::goTop()   , ::loadRow(), ::populateGets() } )
   ENDWITH
   WITH OBJECT oToolbar := QToolBar()
      :setOrientation( Qt_Horizontal )
      :setIconSize( QSize( 12,12 ) )
      :setMovable( .F. )
      :setFloatable( .F. )
      :setFocusPolicy( Qt_NoFocus )
      //
      :addWidget( oBtn1 )
      :addWidget( oBtn2 )
      :addWidget( oBtn3 )
      :addWidget( oBtn4 )
      :addWidget( oBtn5 )
   ENDWITH

   oVLayout:addWidget( oToolbar )

   k1 := SetKey( K_ALT_S   , {|| oBtn1:click() } )
   k2 := SetKey( K_ALT_DOWN, {|| oBtn2:click() } )
   k3 := SetKey( K_ALT_UP  , {|| oBtn3:click() } )
   k4 := SetKey( K_ALT_PGDN, {|| oBtn4:click() } )
   k5 := SetKey( K_ALT_PGUP, {|| oBtn5:click() } )

   ::loadRow()

   FOR EACH oCol IN ::columns
      @ 1,1 QGET ::aModified[ oCol:__enumIndex() ] PICTURE oCol:Picture CAPTION oCol:heading ;
                           WHEN  {|| iif( HB_ISBLOCK( oCol:preBlock ) , Eval( oCol:preBlock ) , .T. ) } ;
                           VALID {|| iif( HB_ISBLOCK( oCol:postBlock ), Eval( oCol:postBlock ), .T. ) }
   NEXT

   QREAD oFLayout LASTGETBLOCK {|| ::saveRow() }

   ::aGetList := GetList

   oDlg:show()
   IF Empty( ::aPosSize )
      aFrame := __hbqtGetWindowFrameWidthHeight( oDlg )
      oPos := ::oWidget:mapToGlobal( QPoint( ::oWidget:width(), 0 ) )
      oDlg:move( oPos:x() + aFrame[ 1 ] / 2, oPos:y() - aFrame[ 2 ] + aFrame[ 1 ] / 2 )
      oDlg:resize( oDlg:width(), ::oWidget:height() )
   ELSE
      oDlg:setGeometry( ::aPosSize[ 3 ] )
      oDlg:move( ::aPosSize[ 1 ], ::aPosSize[ 2 ] )
   ENDIF

   oDlg:exec()

   ::aPosSize := {}
   ASize( ::aPosSize, 5 )
   ::aPosSize[ 1 ] := oDlg:pos():x()
   ::aPosSize[ 2 ] := oDlg:pos():y()
   ::aPosSize[ 3 ] := oDlg:geometry()
   ::aPosSize[ 4 ] := oDlg:width()
   ::aPosSize[ 5 ] := oDlg:height()

   SetKey( K_ALT_S   , k1 )
   SetKey( K_ALT_DOWN, k2 )
   SetKey( K_ALT_UP  , k3 )
   SetKey( K_ALT_PGDN, k4 )
   SetKey( K_ALT_PGUP, k5 )

   oDlg:setParent( QWidget() )

   RETURN NIL


METHOD HbQtBrowse:loadRow()
   LOCAL oCol

   ::aOriginal := {}
   ::aCaptions := {}
   FOR EACH oCol IN ::columns
      AAdd( ::aOriginal, Eval( oCol:block ) )
      AAdd( ::aCaptions, oCol:heading )
   NEXT
   ::aModified := AClone( ::aOriginal )

   RETURN NIL


METHOD HbQtBrowse:saveRow()
   LOCAL lHandelled, oCol

   IF HB_ISBLOCK( ::editBlock() )
      lHandelled := Eval( ::editBlock(), ::aModified, { ::aOriginal, ::aCaptions }, Self )  /* User can RETURN any type */
   ENDIF
   lHandelled := iif( HB_ISLOGICAL( lHandelled ), lHandelled, .F. )
   IF ! lHandelled
      FOR EACH oCol IN ::columns
         Eval( oCol:block, ::aModified[ oCol:__enumIndex() ] )
      NEXT
   ENDIF
   ::refreshCurrent()

   IF ::lDownAfterSave
      ::down()
      ::loadRow()
      ::populateGets()
   ENDIF

   RETURN NIL


METHOD HbQtBrowse:populateGets()
   LOCAL oGet

   FOR EACH oGet IN ::aGetList
      oGet:varPut( ::aModified[ oGet:__enumIndex() ] )
      oGet:display()
   NEXT

   RETURN NIL


METHOD HbQtBrowse:mangageEditorKeyPress( oKeyEvent )

   SWITCH oKeyEvent:key()
   CASE K_ALT_S
      ::save()
      EXIT
   CASE K_ALT_UP
      ::up()
      ::loadRow()
      ::populateGets()
      EXIT
   CASE K_ALT_DOWN
      ::down()
      ::loadRow()
      ::populateGets()
      EXIT
   CASE K_ALT_PGUP
      ::goTop()
      ::loadRow()
      ::populateGets()
      EXIT
   CASE K_ALT_PGDN
      ::goBottom()
      ::loadRow()
      ::populateGets()
      EXIT
   ENDSWITCH

   RETURN .F.


METHOD HbQtBrowse:search( xValue )
   LOCAL oCol := ::getColumn( ::colPos )
   LOCAL GetList := {} , SayList := {}, oDlg

   oDlg := QDialog( ::oWidget )
   oDlg:setWindowTitle( oCol:heading )

   IF xValue == NIL
      ::xSearchValue := Eval( oCol:block )
   ELSE
      ::xSearchValue := xValue
   ENDIF

   @ 1,2 QSAY "Find:" QGET ::xSearchValue PICTURE oCol:picture
   QREAD PARENT oDlg LASTGETBLOCK {|| oDlg:done( 1 ), ::execSearch() }
   ::oSearchGet  := GetList[ 1 ]

   oDlg:exec()
   oDlg:setParent( QWidget() )

   RETURN NIL


METHOD HbQtBrowse:execSearch()

   IF HB_ISBLOCK( ::searchBlock )
      Eval( ::searchBlock, ::xSearchValue, ::colPos, Self )
   ENDIF

   RETURN .T.


METHOD HbQtBrowse:manageContextMenu( oPos )
   LOCAL oPoint := ::oTableView:mapToGlobal( QPoint( oPos ) )
   LOCAL oMenu

   IF HB_ISBLOCK( ::bContextMenuBlock )
      Eval( ::bContextMenuBlock, oPoint, NIL, Self )
   ELSE
      oMenu := QMenu( ::oWidget )
      oMenu:addAction( ::oActSearch )
      oMenu:addSeparator()
      oMenu:addAction( ::oActGoTop )
      oMenu:addAction( ::oActGoBottom )
      oMenu:addAction( ::oActPanHome )
      oMenu:addAction( ::oActPanEnd )
      oMenu:addSeparator()
      oMenu:addAction( ::oActToColumnM )

      oMenu:exec( oPoint )
      oMenu:setParent( QWidget() )
   ENDIF

   RETURN NIL


METHOD HbQtBrowse:contextMenuBlock( bBlock )

   IF HB_ISOBJECT( bBlock )
     ::bContextMenuBlock := bBlock
   ENDIF

   RETURN ::bContextMenuBlock

/*----------------------------------------------------------------------*/

METHOD HbQtBrowse:buildToolbar()

   WITH OBJECT ::oToolbar := QToolBar( ::oWidget )
      :setOrientation( Qt_Horizontal )
      :setIconSize( QSize( 12,12 ) )
      :setMovable( .F. )
      :setFloatable( .F. )
      :setFocusPolicy( Qt_NoFocus )
      //
      :addAction( ::oActEdit )
      :addAction( ::oActPrint )
      :addAction( ::oActSearch )
      :addSeparator()
      :addAction( ::oActGoTop )
      :addAction( ::oActGoUp )
      :addAction( ::oActGoDown )
      :addAction( ::oActGoBottom )
      :addSeparator()
      :addAction( ::oActPanHome )
      :addAction( ::oActLeft )
      :addAction( ::oActRight )
      :addAction( ::oActPanEnd )
      :addSeparator()
      :addAction( ::oActToColumn )
      :addSeparator()
      :addAction( ::oActMoveToFirst )
      :addAction( ::oActMoveToLeft )
      :addAction( ::oActMoveToRight )
      :addAction( ::oActMoveToLast )
      //
      :addSeparator()
      :addAction( ::oActFreezeLPlus )
      :addAction( ::oActFreezeLMinus )
      :addAction( ::oActFreezeRPlus )
      :addAction( ::oActFreezeRMinus )
      :addSeparator()

      :hide()
   ENDWITH

   RETURN Self


METHOD HbQtBrowse:buildActions()

   WITH OBJECT ::oComboColumn := QComboBox()
      :setFocusPolicy( Qt_NoFocus )
      :setTooltip( "Scroll to column..." )
   ENDWITH
   ::oComboColumn:connect( "activated(QString)", {|cColumn| ::toColumn( cColumn ) } )

   WITH OBJECT ::oActToColumn := QWidgetAction( ::oWidget )
      :setDefaultWidget( ::oComboColumn )
      :setTooltip( "Scroll to column..." )
   ENDWITH

   WITH OBJECT ::oComboColumnM := QComboBox()
      :setFocusPolicy( Qt_NoFocus )
      :setTooltip( "Scroll to column..." )
   ENDWITH
   ::oComboColumnM:connect( "activated(QString)", {|cColumn| ::toColumn( cColumn ) } )

   WITH OBJECT ::oActToColumnM := QWidgetAction( ::oWidget )
      :setDefaultWidget( ::oComboColumnM )
      :setTooltip( "Scroll to column..." )
   ENDWITH

   WITH OBJECT ::oActEdit := QAction( ::oWidget )
      :setText( "Edit" )
      :setIcon( QIcon( __hbqtImage( "data-edit" ) ) )
      :setTooltip( "Edit current record" )
      :connect( "triggered()", {|| ::edit() } )
   ENDWITH
   WITH OBJECT ::oActSave := QAction( ::oWidget )
      :setText( "Save" )
      :setIcon( QIcon( __hbqtImage( "save3" ) ) )
      :setTooltip( "Save current record" )
      :connect( "triggered()", {|| ::saveRow() } )
   ENDWITH
   WITH OBJECT ::oActPrint := QAction( ::oWidget )
      :setText( "Print" )
      :setIcon( QIcon( __hbqtImage( "print" ) ) )
      :setTooltip( "Print browser" )
      :connect( "triggered()", {|| ::print() } )
   ENDWITH
   WITH OBJECT ::oActSearch := QAction( ::oWidget )
      :setText( "Search" )
      :setIcon( QIcon( __hbqtImage( "find" ) ) )
      :setTooltip( "Search a value" )
      :connect( "triggered()", {|| ::search() } )
   ENDWITH

   WITH OBJECT ::oActGoUp := QAction( ::oWidget )
      :setText( "Up" )
      :setIcon( QIcon( __hbqtImage( "go-up" ) ) )
      :setTooltip( "Go to previou record" )
      :connect( "triggered()", {|| ::up() } )
   ENDWITH
   WITH OBJECT ::oActGoDown := QAction( ::oWidget )
      :setText( "Down" )
      :setIcon( QIcon( __hbqtImage( "go-down" ) ) )
      :setTooltip( "Go to the next record" )
      :connect( "triggered()", {|| ::down() } )
   ENDWITH
   WITH OBJECT ::oActGoTop := QAction( ::oWidget )
      :setText( "Top" )
      :setIcon( QIcon( __hbqtImage( "go-top" ) ) )
      :setTooltip( "Go to the top record" )
      :connect( "triggered()", {|| ::goTop() } )
   ENDWITH
   WITH OBJECT ::oActGoBottom := QAction( ::oWidget )
      :setText( "Bottom" )
      :setIcon( QIcon( __hbqtImage( "go-bottom" ) ) )
      :setTooltip( "Go to bottom record" )
      :connect( "triggered()", {|| ::goBottom() } )
   ENDWITH

   WITH OBJECT ::oActLeft := QAction( ::oWidget )
      :setText( "Left" )
      :setIcon( QIcon( __hbqtImage( "go-prev" ) ) )
      :setTooltip( "To left column" )
      :connect( "triggered()", {|| ::left() } )
   ENDWITH
   WITH OBJECT ::oActRight := QAction( ::oWidget )
      :setText( "Right" )
      :setIcon( QIcon( __hbqtImage( "go-next" ) ) )
      :setTooltip( "To right column" )
      :connect( "triggered()", {|| ::right() } )
   ENDWITH
   WITH OBJECT ::oActPanHome := QAction( ::oWidget )
      :setText( "Home" )
      :setIcon( QIcon( __hbqtImage( "go-first" ) ) )
      :setTooltip( "Jump to left-most column" )
      :connect( "triggered()", {|| ::panHome() } )
   ENDWITH
   WITH OBJECT ::oActPanEnd := QAction( ::oWidget )
      :setText( "End" )
      :setIcon( QIcon( __hbqtImage( "go-last" ) ) )
      :setTooltip( "Jump to right-most column" )
      :connect( "triggered()", {|| ::panEnd() } )
   ENDWITH

   WITH OBJECT ::oActMoveToLeft := QAction( ::oWidget )
      :setText( "Move Left" )
      :setIcon( QIcon( __hbqtImage( "navigate-left" ) ) )
      :setTooltip( "Move current column left one column" )
      :connect( "triggered()", {|| ::moveLeft() } )
   ENDWITH

   ::oActMoveToRight := QAction( ::oWidget )
   ::oActMoveToRight:setText( "Move Right" )
   ::oActMoveToRight:setIcon( QIcon( __hbqtImage( "navigate-right" ) ) )
   ::oActMoveToRight:setTooltip( "Move current column right one column" )
   ::oActMoveToRight:connect( "triggered()", {|| ::moveRight() } )

   WITH OBJECT ::oActMoveToFirst := QAction( ::oWidget )
      :setText( "Move First" )
      :setIcon( QIcon( __hbqtImage( "navigate-left-most" ) ) )
      :setTooltip( "Move current column at first position" )
      :connect( "triggered()", {|| ::moveHome() } )
   ENDWITH
   WITH OBJECT ::oActMoveToLast := QAction( ::oWidget )
      :setText( "Move Last" )
      :setIcon( QIcon( __hbqtImage( "navigate-right-most" ) ) )
      :setTooltip( "Move current column at last position" )
      :connect( "triggered()", {|| ::moveEnd() } )
   ENDWITH

   WITH OBJECT ::oActFreezeLPlus := QAction( ::oWidget )
      :setText( "FreezePlus" )
      :setIcon( QIcon( __hbqtImage( "plus-1" ) ) )
      :setTooltip( "Freeze another column at left" )
      :connect( "triggered()", {|| ::freeze++ } )
   ENDWITH
   WITH OBJECT ::oActFreezeLMinus := QAction( ::oWidget )
      :setText( "FreezeMinus" )
      :setIcon( QIcon( __hbqtImage( "minus-1" ) ) )
      :setTooltip( "UnFreeze another column at left" )
      :connect( "triggered()", {|| ::freeze-- } )
   ENDWITH
   WITH OBJECT ::oActFreezeRPlus := QAction( ::oWidget )
      :setText( "FreezeRPlus" )
      :setIcon( QIcon( __hbqtImage( "plus-2" ) ) )
      :setTooltip( "Freeze another column at right" )
      :connect( "triggered()", {|| ::rfreeze++ } )
   ENDWITH
   WITH OBJECT ::oActFreezeRMinus := QAction( ::oWidget )
      :setText( "FreezeRMinus" )
      :setIcon( QIcon( __hbqtImage( "minus-2" ) ) )
      :setTooltip( "UnFreeze another column at right" )
      :connect( "triggered()", {|| ::rfreeze-- } )
   ENDWITH

   RETURN Self


METHOD HbQtBrowse:toColumn( cColumn )
   LOCAL oCol

   ::panHome()
   FOR EACH oCol IN ::columns
      IF oCol:heading == cColumn
         EXIT
      ENDIF
      ::Right()
   NEXT

   RETURN Self


METHOD HbQtBrowse:manageStatusbar( lShow )
   IF HB_ISLOGICAL( lShow )
      IF lShow
         ::oStatusbar:show()
      ELSE
         ::oStatusbar:hide()
      ENDIF
   ENDIF
   RETURN ::oStatusbar:isVisible()

METHOD HbQtBrowse:manageToolbar( lShow )
   IF HB_ISLOGICAL( lShow )
      IF lShow
         ::oToolbar:show()
      ELSE
         ::oToolbar:hide()
      ENDIF
   ENDIF
   RETURN ::oToolbar:isVisible()


METHOD HbQtBrowse:getTitle()
   RETURN ::xTitle

METHOD HbQtBrowse:setTitle( xTitle )
   IF HB_ISCHAR( xTitle ) .OR. HB_ISARRAY( xTitle ) .OR. HB_ISBLOCK( xTitle )
      ::xTitle := xTitle
   ENDIF
   RETURN ::xTitle


METHOD HbQtBrowse:print( cPrinter, lOpenPrintDialog )
   LOCAL i, oPrinterInfo, oList, oPrinter

   hb_default( @lOpenPrintDialog, .T. )

   IF HB_ISCHAR( cPrinter ) .AND. ! Empty( cPrinter )
      oPrinterInfo := QPrinterInfo()
      oList := oPrinterInfo:availablePrinters()
      FOR i := 0 TO oList:size() - 1
         IF cPrinter == oList:at( i ):printerName()
            oPrinter := QPrinter( oList:at( i ) )
         ENDIF
      NEXT
   ENDIF

#if 0    /* Not known yet, application crashed IF QPrintDialog is opened and returned printer is used */
   IF Empty( oPrinter )
      //oDlg := QPrintDialog( ::oWidget )
      oDlg := QPrintDialog()
   ELSEIF lOpenPrintDialog
      //oDlg := QPrintDialog( oPrinter, ::oWidget )
      oDlg := QPrintDialog( oPrinter )
   ENDIF
   IF HB_ISOBJECT( oDlg )
      oDlg:connect( "accepted(QPrinter*)", {|oPrn| oPrinter := oPrn } )
      oDlg:exec()
   ENDIF

   HB_TRACE( HB_TR_ALWAYS, __objGetClsName( oPrinter ) )
#endif

   IF Empty( oPrinter )
      oPrinter := QPrinter()
      oPrinter:setOutputFormat( QPrinter_PdfFormat )     /* Until issue WITH QPrintDialog() is resolved, Printing will CREATE a .PDF file on disk */
      oPrinter:setOrientation( QPrinter_Portrait )
      oPrinter:setPaperSize( QPrinter_A4 )
//    oPrinter:setFullPage( .T. )
   ENDIF

   ::printPreview( oPrinter )

#if 0
   IF HB_ISOBJECT( oDlg )
//    oDlg:setParent( QWidget() )
   ENDIF
#endif

   RETURN Self


METHOD HbQtBrowse:printPreview( oPrinter )
   LOCAL oDlg

   ::oPenBlack := QPen( Qt_black, 1, Qt_SolidLine, Qt_SquareCap, Qt_BevelJoin )

   oDlg := QPrintPreviewDialog( oPrinter )//, ::oWidget )
   oDlg:connect( "paintRequested(QPrinter*)", {|p| ::paintRequested( p ) } )

   oDlg:setWindowTitle( "TBrowse Printed" )
   oDlg:move( 20, 20 )
   oDlg:resize( 400, 600 )
   oDlg:exec()
// oDlg:setParent( QWidget() )

   RETURN NIL


METHOD HbQtBrowse:paintRequested( oPrinter )
   ::printReport( oPrinter )
   RETURN Self


METHOD HbQtBrowse:printReport( oPrinter )
   LOCAL oPainter, i, oFM, nH, nW, nRH, nAW, nRows, nCols, nT, n, aX, nX, cVal, nM, nF, xTmp
   LOCAL oPage, oPaper, nMX, nMY, nML, nMT, nPxlX, nPxlW, nTtlMax, nPage, aLen, nLeading
   LOCAL aTitle := {}
   LOCAL nCGap := 2

   IF HB_ISCHAR( ::title )
      aTitle := hb_ATokens( ::title, ";" )
   ELSEIF HB_ISARRAY( ::title )
      aTitle := ::title
   ELSEIF HB_ISBLOCK( ::title )
      xTmp := Eval( ::title, NIL,  NIL, Self )
      IF HB_ISCHAR( xTmp )
         aTitle := hb_ATokens( xTmp, ";" )
      ELSEIF HB_ISARRAY( xTmp )
         aTitle := xTmp
      ENDIF
   ENDIF
   IF Empty( aTitle ) .OR. ! HB_ISARRAY( aTitle )
      aTitle := {}
      AAdd( aTitle, "Browsed Data" )
   ENDIF
   IF Len( aTitle ) > 5
      ASize( aTitle, 5 )
   ENDIF
   nTtlMax := 0
   AEval( aTitle, {|e| nTtlMax := Max( nTtlMax, Len( e ) ) } )

   oPainter := QPainter()
   IF ! oPainter:begin( oPrinter )
      Alert( "Printing could not been started!" )
      RETURN NIL
   ENDIF

   oPainter:setFont( ::oFont )

   oPaper := oPrinter:paperRect()
   oPage  := oPrinter:pageRect()

   nMX   := oPaper:width()  - oPage:width()
   nMY   := oPaper:height() - oPage:height()

   nW    := oPrinter:width()  - nMX
   nH    := oPrinter:height() - nMY

   oFM   := QFontMetrics( oPainter:font() )
   nAW   := oFM:averageCharWidth()
   nRH   := oFM:lineSpacing()
   nLeading := oFM:descent() + oFM:leading() + 1

   nRows := Int( nH / nRH )
   nCols := Int( nW / nAW )

   nF := 0
   nX := 0
   aX := {}
   aLen := {}
   AAdd( aX, 0 )
   FOR i := 1 TO ::colCount
      cVal := ::cellValue( 1, i )
      IF nX + Len( cVal ) > nCols
         EXIT
      ENDIF
      nX := nX + Len( cVal ) + nCGap
      AAdd( aLen, Len( cVal ) )
      AAdd( aX, nX )
      nF++
   NEXT
   nX  -= nCGap
   nM  := Int( ( nCols - nX ) / 2 )
   nML := nMX / 2
   nMT := nMY / 2

   nPxlX := nML + ( nM * nAW )
   nPxlW := nX * nAW

   nPage := 1
   nT  := nMT
   nT := ::drawTitle( oPainter, aTitle, nT, nRH, nCols, nPage, nML, aX, nF, aLen, nPxlX, nPxlW, nM, nAW, nLeading )

   /* 1 Top Line, 1 bottom line, title lines - minimum 1 - paging info - headings - line */
   nRows -= 1 + 1 + 1 + Len( aTitle ) + 1 + 1
   n := 0
   ::goTop()
   DO WHILE .T.
      IF n > nRows
         oPainter:setPen( ::oPenBlack )
         oPainter:drawLine( nPxlX, nT-nRH/2, nPxlX + nPxlW, nT-nRH/2 )
         //
         IF ! oPrinter:newPage()
            Alert( "Printer misbehaving, exiting printing!" )
            EXIT
         ENDIF
         //
         nPage++
         nT := nMT
         nT := ::drawTitle( oPainter, aTitle, nT, nRH, nCols, nPage, nML, aX, nF, aLen, nPxlX, nPxlW, nM, nAW, nLeading )
         n := 0
      ENDIF
      ::down()
      IF ::hitBottom
         EXIT
      ENDIF
      FOR i := 1 TO nF
         oPainter:fillRect( nML + ( ( nM + aX[ i ] ) * nAW ), nT-nRH+nLeading, aLen[ i ] * nAW, nRH, ;
                              __hbqtHbColorToQtValue( ::colorValue( ::cellColor( ::rowPos, i )[ 1 ] ), Qt_BackgroundRole ) )
         oPainter:setPen( ::compatColor( __hbqtHbColorToQtValue( ::colorValue( ::cellColor( ::rowPos, i )[ 1 ] ), Qt_ForegroundRole ) ) )
         oPainter:drawText( nML + ( ( nM + aX[ i ] ) * nAW ), nT, ::cellValue( ::rowPos, i ) )
      NEXT
      n++
      nT += nRH
   ENDDO
   IF nT < nH
      oPainter:setPen( ::oPenBlack )
      oPainter:drawLine( nPxlX, nT-nRH/2, nPxlX + nPxlW, nT-nRH/2 )
   ENDIF

   oPainter:end()

   RETURN Self


METHOD HbQtBrowse:drawTitle( oPainter, aTitle, nT, nRH, nCols, nPage, nML, aX, nF, aLen, nPxlX, nPxlW, nM, nAW, nLeading )
   LOCAL cPage := DToC( Date() ) + " " + Time() + " Page:" + hb_ntos( nPage )
   LOCAL cText, i, nn

   oPainter:setPen( ::oPenBlack )

   FOR EACH cText IN aTitle
      oPainter:drawText( nML, nT, PadC( cText, nCols ) )
      nT += nRH
   NEXT
   oPainter:drawText( nML, nT, PadC( cPage, nCols ) )
   nT += nRH
   oPainter:drawLine( nPxlX, nT-nRH/2, nPxlX + nPxlW, nT-nRH/2 )
   nT += nRH

   FOR i := 1 TO nF
      nN := nML + ( ( nM + aX[ i ] ) * nAW )
      oPainter:fillRect( nN, nT-nRH+nLeading-nRH/4, aLen[ i ] * nAW, nRH, QColor( 220,220,220 ) )
      oPainter:drawText( nN, nT-nRH/4, PadC( ::columns[ i ]:heading, aLen[ i ] ) )
   NEXT
   nT += nRH

   oPainter:drawLine( nPxlX, nT-nRH/2, nPxlX + nPxlW, nT-nRH/2 )
   nT += nRH

   RETURN nT
