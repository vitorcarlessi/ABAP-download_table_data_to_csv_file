*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZHR_DOWNLOAD_TABLE                                     *
* Include.....: ZHR_DOWNLOAD_TABLE_SCR                                 *
* Module......: ALL                                                    *
* Description.: Download Table Data to CSV File                        *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
*----------------------------------------------------------------------*
*              Screen                                                  *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_rsrd1  TYPE rsrd1-tbma_val OBLIGATORY,
            p_files  TYPE char1          OBLIGATORY DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b1.