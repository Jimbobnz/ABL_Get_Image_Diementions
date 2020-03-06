
                   
                       
FUNCTION progress_jpeg RETURNS INTEGER EXTENT 2 (INPUT imageData AS MEMPTR):
    
    DEFINE VARIABLE in_offset           AS INTEGER  NO-UNDO.
    DEFINE VARIABLE in_marker           AS INTEGER  NO-UNDO.
    DEFINE VARIABLE in_length           AS INTEGER  NO-UNDO.
    DEFINE VARIABLE in_FileSize         AS INTEGER  NO-UNDO.
    DEFINE VARIABLE in_ImageDimensions  AS INTEGER  NO-UNDO INITIAL ? EXTENT 2 .

    /* JPEG files are all big-endian byte order.*/
    SET-BYTE-ORDER(imageData) = BIG-ENDIAN.

    MESSAGE 'JPEG'.

    in_offset   = 3. /** Ignore the first two bytes.**/
    in_FileSize = GET-SIZE(ImageData).

    Byte-Scan:
    DO WHILE in_offset LT in_FileSize:
                    
        in_marker = GET-UNSIGNED-SHORT(ImageData,in_offset).
        in_length = GET-UNSIGNED-SHORT(ImageData,in_offset + 2).

        IF (in_marker EQ 0xFFC0 OR in_marker EQ 0xFFC2) THEN
        DO:
            ASSIGN               
                in_ImageDimensions[2] = GET-UNSIGNED-SHORT(ImageData,in_offset + 5)  /** Width **/  
                in_ImageDimensions[1] = GET-UNSIGNED-SHORT(ImageData,in_offset + 7). /** Height **/

            LEAVE Byte-Scan.
        END.
    
        in_offset = in_offset + in_length + 2.
    END.

    RETURN in_ImageDimensions.
END FUNCTION.

FUNCTION progress_png RETURNS INTEGER EXTENT 2 (INPUT imageData AS MEMPTR):

    DEFINE VARIABLE in_ImageDimensions  AS INTEGER   NO-UNDO EXTENT 2 INITIAL ?.

    MESSAGE 'PNG'.

    /* PNG file are in network byte order (big-endian) */
    SET-BYTE-ORDER(imageData) = BIG-ENDIAN.

    ASSIGN
        in_ImageDimensions[1] = GET-UNSIGNED-LONG(imageData,17)  /** Width **/
        in_ImageDimensions[2] = GET-UNSIGNED-LONG(imageData,21). /** Height **/
    SET-SIZE(imageData) = 0 .
    RETURN in_ImageDimensions. 
END FUNCTION.

FUNCTION progress_bmp RETURNS INTEGER EXTENT 2 (INPUT imageData AS MEMPTR):
    DEFINE VARIABLE in_ImageDimensions  AS INTEGER   NO-UNDO EXTENT 2 INITIAL ?.
    /** All of the integer values are stored in little-endian format (i.e. least-significant byte first). **/
    SET-BYTE-ORDER(imageData) = LITTLE-ENDIAN.
    MESSAGE 'BMP'.
    
    ASSIGN
        in_ImageDimensions[1] = GET-UNSIGNED-LONG(imageData,19) /** Width **/
        in_ImageDimensions[2] = GET-UNSIGNED-LONG(imageData,23). /** Height **/

    SET-SIZE(imageData) = 0 .
    RETURN in_ImageDimensions.
END FUNCTION.


FUNCTION progress_gif RETURNS INTEGER EXTENT 2 (INPUT imageData AS MEMPTR):
    DEFINE VARIABLE in_ImageDimensions AS INTEGER EXTENT 2 NO-UNDO.
    /** All of the integer values are stored in little-endian format (i.e. least-significant byte first). **/
    SET-BYTE-ORDER(imageData) = LITTLE-ENDIAN.

    MESSAGE 'GIF'.

    ASSIGN
        in_ImageDimensions[1] = GET-SHORT(imageData,7)  /** Width **/
        in_ImageDimensions[2] = GET-SHORT(imageData,9). /** Height **/

    SET-SIZE(imageData) = 0 .
    RETURN in_ImageDimensions.
END FUNCTION.

FUNCTION GetImageDimensions RETURNS INTEGER EXTENT 2 (INPUT ch_filename AS CHARACTER):

    DEFINE VARIABLE imageData           AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE in_ImageDimensions  AS INTEGER   NO-UNDO EXTENT 2 INITIAL ?.
    DEFINE VARIABLE rPNGSignature  AS RAW NO-UNDO. 
    DEFINE VARIABLE rBMPSignature  AS RAW NO-UNDO.
    DEFINE VARIABLE rGIFSIGNATURE  AS RAW NO-UNDO.
    DEFINE VARIABLE rJPEGSignature AS RAW NO-UNDO.
    
    rPNGSignature  = HEX-DECODE('89504E470D0A1A0A':U).
    rBMPSignature  = HEX-DECODE('424D':U).
    rGIFSignature  = HEX-DECODE('47494638':U).
    rJPEGSignature = HEX-DECODE('FFD8FF':U).

    ch_filename = SEARCH(ch_filename).

    IF ch_filename EQ ? THEN
        RETURN in_ImageDimensions.

    SET-SIZE(imageData) = 0.    /** Always reset your MEMPTRs **/
    COPY-LOB FROM FILE ch_filename TO OBJECT imageData.

/************************************/
/* DEBUG CODE ***********************/
/************************************/

/*     DEFINE VARIABLE iBYTE   AS INTEGER     NO-UNDO.                              */
/*     DEFINE VARIABLE imageRAW AS RAW       NO-UNDO.                               */
/*                                                                                  */
/*     OUTPUT STREAM sDebug TO 'hex-encoded.txt'.                                  */
/*                                                                                  */
/*     DO iBYTE = 1 TO GET-SIZE(ImageData):                                         */
/*         imagERAW= GET-BYTES(ImageData,iBYTE,1).                                  */
/*         PUT STREAM sDebug UNFORMATTED TRIM(STRING(HEX-ENCODE(imagERAW))) + ' '. */
/*         IF iBYTE MOD 16 EQ 0 THEN                                                */
/*             PUT STREAM sDebug SKIP.                                             */
/*     END.                                                                         */
/*     OUTPUT STREAM sDebug CLOSE.                                                 */

    /** Magic Number for File Tpye Identification..**/
    CASE TRUE:
        WHEN GET-BYTES(imageData,1,3)  EQ rJPEGSignature THEN  /** JPEG **/
            in_ImageDimensions = progress_jpeg(INPUT imageData ).

        WHEN GET-BYTES(imageData,1,8)  EQ rPNGSignature THEN /** PNG **/
            in_ImageDimensions = progress_png(INPUT imageData ).
    
        WHEN GET-BYTES(imageData,1,2)  EQ rBMPSignature THEN /** BMP **/
            in_ImageDimensions = progress_bmp(INPUT imageData ).

        WHEN GET-BYTES(imageData,1,4) EQ rGIFSignature THEN /** GIF **/
            in_ImageDimensions = progress_gif(INPUT imageData ).
    END CASE.

    SET-SIZE(imageData) = 0.  /** Always reset your MEMPTRs **/

    RETURN in_ImageDimensions.
END FUNCTION.


DEFINE STREAM sDebug.    

DEFINE VARIABLE in_ImageDimensions AS INTEGER NO-UNDO EXTENT 2 INITIAL ?.

 in_ImageDimensions = GetImageDimensions(INPUT "webspeed\images\pscpbp1.gif").            
/* in_ImageDimensions = GetImageDimensions(INPUT "webspeed\samples\internet\cat00029.jpg"). */
/* in_ImageDimensions = GetImageDimentions(INPUT "netsetup\setup.bmp").  */
/* in_ImageDimensions = GetImageDimentions(INPUT "jdk\jre\lib\servicetag\jdk_header.png"). */

MESSAGE 'Width' in_ImageDimensions[1] SKIP 
        'Height' in_ImageDimensions[2]
    VIEW-AS ALERT-BOX INFO.
