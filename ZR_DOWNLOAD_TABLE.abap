*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_DOWNLOAD_TABLE                                      *
* Module......: ALL                                                    *
* Description.: Download Table Data to CSV File                        *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
REPORT zhr_download_table.

INCLUDE zhr_download_table_top.
INCLUDE zhr_download_table_scr.
INCLUDE zhr_download_table_c01.

INITIALIZATION.
  "Class instance
  CREATE OBJECT go_download_table.

START-OF-SELECTION.
  "Start Full Process
  go_download_table->full_process( ).