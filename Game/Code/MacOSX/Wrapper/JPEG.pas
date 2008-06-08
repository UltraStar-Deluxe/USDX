unit JPEG;

{$I switches.inc}

interface

uses Graphics;

type
    TJPEGImage = class(TBitmap)
    private
        FCompressionQuality : Integer;
    public
        Procedure SaveToFile(const f : String); override;
        property CompressionQuality : Integer read FCompressionQuality write FCompressionQuality;
    end;

implementation

uses FreeImage;

{ TJPEGImage }

procedure TJPEGImage.SaveToFile(const f: String);
begin
    if CompressionQuality = 0 then begin
        CompressionQuality := 95;
    end;

    FImage.Save( f, CompressionQuality);
end;

end.
 
