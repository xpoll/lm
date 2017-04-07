package cn.blmdz.rabbit.web.core.image;

import java.io.Serializable;

import com.google.common.base.Objects;

import cn.blmdz.wolf.file.model.UserFile;
import cn.blmdz.wolf.file.model.UserFolder;
import lombok.Data;

@Data
public class UploadDto implements Serializable {

    private static final long serialVersionUID = -38331060124340967L;

    /**
     * 类型(文件夹|文件)
     * @see cn.blmdz.rabbit.web.core.image.UploadDto.Type
     */
    private Integer type;

    /**
     * 文件信息
     */
    private UserFile userFile;

    /**
     * 文件夹信息
     */
    private UserFolder userFolder;

    private String error;

    public UploadDto(){}

    public UploadDto(UserFile userFile){
        this.type = Type.FILE.toNumber();
        this.userFile = userFile;
    }

    public UploadDto(UserFolder userFolder){
        this.type = Type.FOLDER.toNumber();
        this.userFolder = userFolder;
    }

    public UploadDto(UserFile userFile, String error){
        this.type = Type.FILE.toNumber();
        this.userFile = userFile;
        this.error = error;
    }

    public static enum Type {
        FOLDER(1, "文件夹"),
        FILE(2, "文件");

        private final Integer value;

        private final String display;

        private Type(Integer value, String display) {
            this.value = value;
            this.display = display;
        }

        public static Type fromNumber(Integer value) {
            for (Type t : Type.values()) {
                if (Objects.equal(t.value, value)) {
                    return t;
                }
            }
            return null;
        }

        public Integer toNumber() {
            return this.value;
        }

        @Override
        public String toString() {
            return this.display;
        }
    }
}
