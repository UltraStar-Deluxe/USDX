unit opencv_core;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils, opencv_types,
  {$IFDEF WIN32}
    Windows, Dialogs;
  {$ELSE}
    dynlibs;
  {$ENDIF}

var

///* Creates IPL image (header and data) */
  cvCreateImage: function(size: CvSize; depth: integer; channels: integer ): PIplImage;cdecl;
//  cvCreateMemStorage: function( block_size: integer = 0): PCvMemStorage;cdecl;

///*********************************** Adding own types ***********************************/
///
//CVAPI(void) cvRegisterType( const CvTypeInfo* info );
//CVAPI(void) cvUnregisterType( const char* type_name );
//CVAPI(CvTypeInfo*) cvFirstType(void);
//CVAPI(CvTypeInfo*) cvFindType( const char* type_name );
//CVAPI(CvTypeInfo*) cvTypeOf( const void* struct_ptr );
//
///* universal functions */
//CVAPI(void) cvRelease( void** struct_ptr );
//CVAPI(void*) cvClone( const void* struct_ptr );
//
///* simple API for reading/writing data */
//CVAPI(void) cvSave( const char* filename, const void* struct_ptr,
//                    const char* name CV_DEFAULT(NULL),
//                    const char* comment CV_DEFAULT(NULL),
//                    CvAttrList attributes CV_DEFAULT(cvAttrList()));
{
cvLoad:function ( filename: pAnsiChar;
                  memstorage: PCvMemStorage=nil ;
                  name: PansiChar=nil;
                  real_name: PPAnsiChar=nil ): pointer; cdecl;
}
///* Clears memory storage. This is the only way(!!!) (besides cvRestoreMemStoragePos)
//   to reuse memory allocated for the storage - cvClearSeq,cvClearSet ...
//   do not free any memory.
//   A child storage returns all the blocks to the parent when it is cleared */
//CVAPI(void)  cvClearMemStorage( CvMemStorage* storage );
//cvClearMemStorage: procedure( storage: PCvMemStorage); cdecl;

///* Retrieves pointer to specified sequence element.
//   Negative indices are supported and mean counting from the end
//   (e.g -1 means the last sequence element) */
//var cvGetSeqElem: function(seq: PCvSeq; index: integer): pAnsiChar; cdecl;

///* Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2),
//   if thickness<0 (e.g. thickness == CV_FILLED), the filled box is drawn */
//var cvRectangle: procedure( img: pointer; pt1:CvPoint; pt2:CvPoint;
//                          color: CvScalar; thickness: integer = 1;
//                          line_type: integer = 8;
//                          shift: integer = 0); cdecl;


///* Releases (i.e. deallocates) IPL image header */
//var cvReleaseImageHeader: procedure(image: pIplImage);cdecl;

///* Releases IPL image header and data */
var cvReleaseImage: procedure(image: PPIplImage);cdecl;

///* Creates a copy of IPL image (widthStep may differ) */
var cvCloneImage: function(image:pIplImage):PIplImage;cdecl;

///* Creates new empty sequence that will reside in the specified storage */
//var cvCreateSeq: function(seq_flags: integer; header_size: integer;
//                      elem_size: integer; storage: PCvMemStorage):PCvSeq;cdecl;

///* Sets image ROI (region of interest) (COI is not changed) */
//var cvSetImageROI: procedure(image: PIplImage; rect: CvRect); cdecl;

///* Sets a Channel Of Interest (only a few functions support COI) -
//   use cvCopy to extract the selected channel and/or put it back */
//var cvSetImageCOI: procedure(image:PIplImage; coi: integer); cdecl;

///* Copies source array to destination array */
//var cvCopy: procedure(src:PIplImage; dst:PIplImage;
//                  mask: PCvArr = nil); cdecl;

// Invert
//var cvInvert: function( const A : PCvArr; B : PCvArr; method : integer ) : double; cdecl;

//var cvAdd: procedure(src1:PIplImage; src2:PIplImage; dst:PIplImage; mask: CvArr = nil);  cdecl;
var cvAddS: procedure(src:PIplImage; value: CvScalar; dst:PIplImage; mask: CvArr = nil);  cdecl;
var cvNot: procedure(src:PIplImage; dst:PIplImage);  cdecl;
var cvConvertScale: procedure(src:PIplImage; dst:PIplImage; scale: double = 1; shift: double = 0);  cdecl;
var cvSplit: procedure(src: PIplImage; dst0, dst1, dst2, dst3: PIplImage); cdecl;
var cvFlip: procedure(src: PIplImage; dst:PIplImage = nil; flipmode: integer = 0); cdecl;

///* Differences with actual and last image
var cvAbsDiff: procedure(src1:PIplImage; src2:PIplImage; dst:PIplImage);  cdecl;

var cvMerge: procedure (src0, src1, src2, src3:PIplImage; dst:PIplImage);  cdecl;
///* Adds new element to the end of sequence. Returns pointer to the element */
//var cvSeqPush: function(seq:PCvSeq; element: pointer = nil): pansichar; cdecl;

///* Initializes sequence reader.
//   The sequence can be read in forward or backward direction */
//var cvStartReadSeq: procedure(  seq: PCvSeq;  reader: PCvSeqReader;
//                           reverse: integer= 0 );cdecl;


///* Draws one or more polygonal curves */
//var  cvPolyLine: procedure(  img: CvArr; pts:PCvPoint;  npts: Pinteger;  contours: integer;
//                         is_closed: integer; color: CvScalar;  thickness: integer = 1;
//                          line_type: integer = 8; shift: integer = 0); cdecl;

function CV_RGB( r, g, b : double ): CvScalar;

const
  CV_AA = 16;

///************ Internal sequence functions ************/
var cvChangeSeqBlock: procedure( reader: pointer;direction:integer ); cdecl;
//var cvCreateSeqBlock: procedure( writer: PCvSeqWriter );cdecl;
implementation

const
  {$IF Defined(MSWINDOWS)}
    DLL_CXCORE='cxcore210.dll';
  {$ELSEIF Defined(DARWIN)}
    DLL_CXCORE='libopencv_core.dylib';
  {$ELSEIF Defined(UNIX)}
    DLL_CXCORE='libopencv_core.so';
  {$IFEND}

var
  DLLHandle: THandle;

procedure LoadDLL;
begin
  DLLHandle := LoadLibrary(DLL_CXCORE);
  if DLLHandle >= 32 then
  begin
    @cvCreateImage := GetProcAddress(DLLHandle,'cvCreateImage');
    {$IFDEF WIN32}
    Assert(@cvCreateImage <> nil);
    {$ENDIF}
    //
    //@cvCreateMemStorage := GetProcAddress(DLLHandle,'cvCreateMemStorage');
    {$IFDEF WIN32}
    //Assert(@cvCreateMemStorage <> nil);
    {$ENDIF}
    //
    //@cvLoad := GetProcAddress(DLLHandle,'cvLoad');
    {$IFDEF WIN32}
    //Assert(@cvLoad <> nil);
    {$ENDIF}
    //
    //@cvClearMemStorage := GetProcAddress(DLLHandle,'cvClearMemStorage');
    {$IFDEF WIN32}
    //Assert(@cvClearMemStorage <> nil);
    {$ENDIF}
    //
    //@cvGetSeqElem := GetProcAddress(DLLHandle,'cvGetSeqElem');
    {$IFDEF WIN32}
    //Assert(@cvGetSeqElem <> nil);
    {$ENDIF}
    //
    //@cvRectangle := GetProcAddress(DLLHandle,'cvRectangle');
    {$IFDEF WIN32}
    //Assert(@cvRectangle <> nil);
    {$ENDIF}
    //
    @cvReleaseImage := GetProcAddress(DLLHandle,'cvReleaseImage');
    {$IFDEF WIN32}
    Assert(@cvReleaseImage <> nil);
    {$ENDIF}
    //
    @cvCloneImage := GetProcAddress(DLLHandle,'cvCloneImage');
    {$IFDEF WIN32}
    Assert(@cvCloneImage <> nil);
    {$ENDIF}
    //
    //@cvCreateSeq := GetProcAddress(DLLHandle,'cvCreateSeq');
    {$IFDEF WIN32}
    //Assert(@cvCreateSeq <> nil);
    {$ENDIF}
    //
    //@cvSetImageROI := GetProcAddress(DLLHandle,'cvSetImageROI');
    {$IFDEF WIN32}
    //Assert(@cvSetImageROI <> nil);
    {$ENDIF}
    //
    //@cvSetImageCOI := GetProcAddress(DLLHandle,'cvSetImageCOI');
    {$IFDEF WIN32}
    //Assert(@cvSetImageCOI <> nil);
    {$ENDIF}
    //
    //@cvCopy := GetProcAddress(DLLHandle,'cvCopy');
    {$IFDEF WIN32}
    //Assert(@cvCopy <> nil);
    {$ENDIF}
    //
    //@cvSeqPush := GetProcAddress(DLLHandle,'cvSeqPush');
    {$IFDEF WIN32}
    //Assert(@cvSeqPush <> nil);
    {$ENDIF}
    //
    //@cvStartReadSeq := GetProcAddress(DLLHandle,'cvStartReadSeq');
    {$IFDEF WIN32}
    //Assert(@cvStartReadSeq <> nil);
    {$ENDIF}
    //
    //@cvStartReadSeq := GetProcAddress(DLLHandle,'cvStartReadSeq');
    {$IFDEF WIN32}
    //Assert(@cvStartReadSeq <> nil);
    {$ENDIF}

    //
    //@cvPolyLine := GetProcAddress(DLLHandle,'cvPolyLine');
    {$IFDEF WIN32}
    //Assert(@cvPolyLine <> nil);
    {$ENDIF}
    //
    //@cvChangeSeqBlock := GetProcAddress(DLLHandle,'cvChangeSeqBlock');
    {$IFDEF WIN32}
    //Assert(@cvChangeSeqBlock <> nil);
    {$ENDIF}

    //
    @cvAbsDiff := GetProcAddress(DLLHandle,'cvAbsDiff');
    {$IFDEF WIN32}
    Assert(@cvAbsDiff <> nil);
    {$ENDIF}

    //
    //@cvAdd := GetProcAddress(DLLHandle,'cvAdd');
    {$IFDEF WIN32}
    //Assert(@cvAdd <> nil);
    {$ENDIF}

    //
    @cvAddS := GetProcAddress(DLLHandle,'cvAddS');
    {$IFDEF WIN32}
    Assert(@cvAddS <> nil);
    {$ENDIF}

    //
    @cvNot := GetProcAddress(DLLHandle,'cvNot');
    {$IFDEF WIN32}
    Assert(@cvNot <> nil);
    {$ENDIF}

    //
    @cvConvertScale := GetProcAddress(DLLHandle,'cvConvertScale');
    {$IFDEF WIN32}
    Assert(@cvConvertScale <> nil);
    {$ENDIF}

    //
    @cvSplit := GetProcAddress(DLLHandle,'cvSplit');
    {$IFDEF WIN32}
    Assert(@cvSplit <> nil);
    {$ENDIF}

        //
    @cvFlip := GetProcAddress(DLLHandle,'cvFlip');
    {$IFDEF WIN32}
    Assert(@cvFlip <> nil);
    {$ENDIF}

    //
    //@cvInvert := GetProcAddress(DLLHandle,'cvInvert');
    //{$IFDEF WIN32}
    //Assert(@cvInvert <> nil);
    //{$ENDIF}

    //
    @cvMerge := GetProcAddress(DLLHandle,'cvMerge');
    {$IFDEF WIN32}
    Assert(@cvMerge <> nil);
    {$ENDIF}
  end
  else
  begin
    //showmessage(format('Error: %s could not be loaded !!',[DLL_CXCORE]));
    { Error: WINUSB.DLL could not be loaded !! }
  end;
end;

function CV_RGB( r, g, b : double ): CvScalar;
begin
  result.val[0] := b;
  result.val[1] := g;
  result.val[2] := r;
  result.val[3] := 0;
end;


initialization
  LoadDLL;


finalization
;

end.
