package io.terminus.galaxy.web.core.util;

import lombok.Setter;
import net.glxn.qrgen.QRCode;
import net.glxn.qrgen.image.ImageType;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.OutputStream;

/**
 * Desc: 二维码生成工具
 * Mail: houly@terminus.io
 * author: Hou Luyao
 * Date: 15/1/26.
 */
@Component
public class QrHelper {
    @Setter
    private int width = 250;
    @Setter
    private int height = 250;
    
    public File getQrCode(String content) {
        return getQrCode(content, width, height);
    }


    public File getQrCode(String content, int width, int height) {
        return getQrCode(content, width, height, ImageType.GIF);
    }


    public File getQrCode(String content, int width, int height, ImageType imageType) {
        return QRCode.from(content).to(imageType).withSize(width, height).file();
    }


    public OutputStream getQrCodeStream(String content) {
        return getQrCodeStream(content, width, height);
    }


    public OutputStream getQrCodeStream(String content, int width, int height) {
        return getQrCodeStream(content, width, height, ImageType.GIF);
    }


    public OutputStream getQrCodeStream(String content, int width, int height, ImageType imageType) {
        return QRCode.from(content).to(imageType).withSize(width, height).stream();
    }
}
