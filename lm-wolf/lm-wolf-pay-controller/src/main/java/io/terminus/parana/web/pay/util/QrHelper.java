package io.terminus.parana.web.pay.util;

import java.io.File;
import java.io.OutputStream;
import net.glxn.qrgen.QRCode;
import net.glxn.qrgen.image.ImageType;

public class QrHelper {
   private int width = 250;
   private int height = 250;

   public File getQrCode(String content) {
      return this.getQrCode(content, this.width, this.height);
   }

   public File getQrCode(String content, int width, int height) {
      return this.getQrCode(content, width, height, ImageType.GIF);
   }

   public File getQrCode(String content, int width, int height, ImageType imageType) {
      return QRCode.from(content).to(imageType).withSize(width, height).file();
   }

   public OutputStream getQrCodeStream(String content) {
      return this.getQrCodeStream(content, this.width, this.height);
   }

   public OutputStream getQrCodeStream(String content, int width, int height) {
      return this.getQrCodeStream(content, width, height, ImageType.GIF);
   }

   public OutputStream getQrCodeStream(String content, int width, int height, ImageType imageType) {
      return QRCode.from(content).to(imageType).withSize(width, height).stream();
   }

   public void setWidth(int width) {
      this.width = width;
   }

   public void setHeight(int height) {
      this.height = height;
   }
}
