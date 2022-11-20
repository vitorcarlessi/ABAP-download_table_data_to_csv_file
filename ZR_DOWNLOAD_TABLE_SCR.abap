*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_DOWNLOAD_TABLE                                      *
* Include.....: ZR_DOWNLOAD_TABLE_SCR                                  *
* Module......: ALL                                                    *
* Description.: Download Table Data to CSV File                        *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
*----------------------------------------------------------------------*
*              Screen                                                  *
*----------------------------------------------------------------------*
TABLES: rsrd1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: s_rsrd1 FOR  rsrd1-tbma_val OBLIGATORY NO INTERVALS.
PARAMETERS:     p_csv_p TYPE localfile      OBLIGATORY.
PARAMETERS:     p_files TYPE char1          OBLIGATORY DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b1.