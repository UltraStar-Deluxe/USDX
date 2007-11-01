unit Graphics;

{$I switches.inc}

interface

uses
    Classes, SysUtils, Windows, FreeBitmap, FreeImage;

type
    TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
    TColor = -$7FFFFFFF-1..$7FFFFFFF;

    TCanvas = class
    private
        FImage : TFreeBitmap;
        function GetPixel(x, y: Integer): TColor;
        procedure SetPixel(x, y: Integer; const Value: TColor);
    public
        Constructor Create(const bmp : TFreeBitmap);
        property Pixels[x,y : Integer] : TColor read GetPixel write SetPixel;
    end;

    TBitmap = class
    private
        FCanvas : TCanvas;
        function GetHeight: Integer;
        function GetWidth: Integer;
        procedure SetHeight(const Value: Integer);
        procedure SetWidth(const Value: Integer);
        function GetPixelFormat: TPixelFormat;
        procedure SetPixelFormat(const Value: TPixelFormat);
        function GetScanLine(Line: Integer): Pointer;
    protected
        FImage : TFreeBitmap;
    public
        Constructor Create;
        Destructor Destroy; override;
        Procedure LoadFromStream(const str : TStream);
        Procedure LoadFromFile(const f : String);
        Procedure SaveToFile(const f : String); virtual;
        Procedure Assign(const src : TBitmap);
        property Width : Integer read GetWidth write SetWidth;
        property Height : Integer read GetHeight write SetHeight;
        property PixelFormat : TPixelFormat read GetPixelFormat write SetPixelFormat;
        property ScanLine[Line : Integer] : Pointer read GetScanLine;
        property Canvas : TCanvas read FCanvas;
    end;

implementation

{ TBitmap }

type
    TRealRGB = packed record
        rgbRed: Byte;
        rgbGreen: Byte;
        rgbBlue: Byte;
        rgbReserved: Byte;
    end;

procedure TBitmap.Assign(const src: TBitmap);
begin
    FImage.Assign(src.FImage);
    FCanvas.FImage := FImage;
end;

constructor TBitmap.Create;
begin
    FImage  := TFreeBitmap.Create( FIF_BMP, 4, 4, 24);
    FCanvas := TCanvas.Create(FImage);
end;

destructor TBitmap.Destroy;
begin
    FCanvas.Free;
    FImage.Free;
    inherited;
end;

function TBitmap.GetHeight: Integer;
begin
    Result := FImage.GetHeight;
end;

function TBitmap.GetPixelFormat: TPixelFormat;
begin
    Result := pf24bit;

    case FImage.GetBitsPerPixel of
        1  : Result := pf1bit;
        4  : Result := pf4bit;
        8  : Result := pf8bit;
        15 : Result := pf15bit;
        16 : Result := pf16bit;
        24 : Result := pf24bit;
        32 : Result := pf32bit;
    end;
end;

function TBitmap.GetScanLine(Line: Integer): Pointer;
begin
    Result := FImage.GetScanLine(Height-1-Line);
end;

function TBitmap.GetWidth: Integer;
begin
    Result := FImage.GetWidth;
end;

procedure TBitmap.LoadFromFile(const f: String);
begin
    FreeAndNil(FCanvas);
    FreeAndNil(FImage);
    FImage := TFreeBitmap.Create;
    FImage.Load(f);
    FCanvas := TCanvas.Create(FImage);
end;

procedure TBitmap.LoadFromStream(const str: TStream);
begin
    FreeAndNil(FCanvas);
    FreeAndNil(FImage);
    FImage := TFreeBitmap.Create;
    FImage.LoadFromStream(str);
    FCanvas := TCanvas.Create(FImage);
end;

procedure TBitmap.SaveToFile(const f: String);
begin
    FImage.Save(f);
end;

procedure TBitmap.SetHeight(const Value: Integer);
begin
    if Value <> Height then begin
        FImage.Rescale( Width, Value, FILTER_BILINEAR);
    end;
end;

procedure TBitmap.SetPixelFormat(const Value: TPixelFormat);
begin
    if Value <> PixelFormat then begin
        case Value of
            pf4bit  : FImage.ConvertTo4Bits;
            pf8bit  : FImage.ConvertTo8Bits;
            pf15bit : FImage.ConvertTo16Bits555;
            pf16bit : FImage.ConvertTo16Bits565;
            pf24bit : FImage.ConvertTo24Bits;
            pf32bit : FImage.ConvertTo32Bits;
        end;
    end;
end;

procedure TBitmap.SetWidth(const Value: Integer);
begin
    if Value <> Width then begin
        FImage.Rescale( Value, Height, FILTER_BILINEAR);
    end;
end;

{ TCanvas }

constructor TCanvas.Create(const bmp: TFreeBitmap);
begin
    FImage := bmp;
end;

function TCanvas.GetPixel(x, y: Integer): TColor;
var
    pix : TRGBQuad;
begin
    FImage.GetPixelColor( x, FImage.GetHeight-1-y, @pix);
    Result := TColor(pix);
end;

procedure TCanvas.SetPixel(x, y: Integer; const Value: TColor);
var
    pixRGB : TRealRGB;
    pixBGR : TRGBQuad;
begin
    Move( Value, pixRGB, SizeOf(pixRGB));
    pixBGR.rgbRed      := pixRGB.rgbRed;
    pixBGR.rgbGreen    := pixRGB.rgbGreen;
    pixBGR.rgbBlue     := pixRGB.rgbBlue;
    pixBGR.rgbReserved := pixRGB.rgbReserved;
    FImage.SetPixelColor( x, FImage.GetHeight-1-y, @pixBGR);
end;

end.
