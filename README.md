# ABL Get Image Diementions
ABL Get Image Dimensions
Simple ABL collection of functions to be able exstract image dimensions from image files

```
DEFINE VARIABLE in_ImageDimensions AS INTEGER NO-UNDO EXTENT 2 INITIAL ?.

 in_ImageDimensions = GetImageDimensions(INPUT "webspeed\images\pscpbp1.gif").            

MESSAGE 'Width' in_ImageDimensions[1] SKIP 
        'Height' in_ImageDimensions[2]
    VIEW-AS ALERT-BOX INFO.
```
