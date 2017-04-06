package io.terminus.galaxy.user.model;

import lombok.Data;
import org.joda.time.DateTime;

import java.io.Serializable;
import java.util.Date;

import static io.terminus.common.utils.Arguments.equalWith;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
@Data
public class Notice implements Serializable {

    private static final long serialVersionUID = 205398509315403185l;
    /**
     * 主键
     */
    private Long id;
    /**
     * 标题
     */
    private String title;
    /**
     * 1->商家, 2->平台
     */
    private Integer level;
    /**
     * 公告类型 1:新闻 2:公告
     */
    private Integer type;
    /**
     * 新闻摘要
     */
    private String summary;
    /**
     * 店铺ID
     */
    private Long shopId;
    /**
     * 状态，1->已发布2->停止
     */
    private Integer status;
    /**
     * 内容
     */
    private String context;
    /**
     * 创建者ID
     */
    private Long creatorId;
    /**
     * 创建者名字
     */
    private String creatorName;
    /**
     * 生效时间
     */
    private Date startAt;
    /**
     * 失效时间
     */
    private Date endAt;
    /**
     * 图片地址
     */
    private String picUrl;
    /**
     * 额外信息
     */
    private String extra;
    protected Date createdAt;             //创建时间
    protected Date updatedAt;

    public static enum Status {
        INIT(0, "初始化"),
        RELEASE(1, "发布"),
        STOP(2, "停止"),
        DELETE(3,"删除");

        private final Integer value;

        private final String display;

        private Status(Integer value, String display) {
            this.value = value;
            this.display = display;
        }

        public static Status fromNumber(Integer value) {
            for (Status t : Status.values()) {
                if (equalWith(t.value, value)) {
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

    /**
     * 判断公告现在是否在有效时间范围内
     */
    public Boolean isCurrentEffective() {
        DateTime now = DateTime.now();
        return now.isAfter(new DateTime(this.startAt))
                && now.isBefore(new DateTime(this.endAt));
    }

    /**
     * 判断公告是否处于某种状态
     */
    public Boolean isStatus(Status s) {
        return equalWith(this.status, s.toNumber());
    }

    /**
     * 判断公告是否可以使用（即已发布且当前时间在有效时间范围内）
     */
    public Boolean canUsed() {
        return !isStatus(Status.INIT) && isCurrentEffective();
    }

}
