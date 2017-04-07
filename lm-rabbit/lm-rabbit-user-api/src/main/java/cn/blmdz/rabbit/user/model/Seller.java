package cn.blmdz.rabbit.user.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;
import lombok.AccessLevel;
import lombok.Data;
import lombok.Setter;
import lombok.SneakyThrows;

/**
 * 卖家
 *
 * @author Effet
 */
@Data
public class Seller implements Serializable {

    private static final long serialVersionUID = 0;

    private static final ObjectMapper OBJECT_MAPPER = JsonMapper.nonEmptyMapper().getMapper();

    /**
     * 主键 ID
     */
    private Long id;

    /**
     * 卖家用户 ID
     */
    private Long userId;

    /**
     * 卖家用户名
     */
    private String userName;

    /**
     * 拥有的店铺 ID
     */
    private Long shopId;

    /**
     * 店铺名
     */
    private String shopName;

    /**
     * admin授权业务
     */
    private String adminBusinessIds;

    /**
     * seller选择业务
     */
    private String sellerBusinessIds;

    /**
     * 0: 未生效(待审核), 1: 生效(审核通过), -1: 审核不通过, -2: 冻结, -3: 删除
     */
    private Integer status;

    public boolean isActive() {
        return status != null && status == 1;
    }

    /**
     * 扩展信息, 不存数据库
     */
    @Setter(AccessLevel.NONE)
    private Map<String, String> extra;

    /**
     * 扩展信息 JSON, 存数据库
     */
    @Setter(AccessLevel.NONE)
    @JsonIgnore
    private String extraJson;

    /**
     * 创建时间
     */
    private Date createdAt;

    /**
     * 更新时间
     */
    private Date updatedAt;

    @SneakyThrows
    public void setExtra(Map<String, String> extra) {
        this.extra = extra;
        if (extra == null || extra.isEmpty()) {
            this.extraJson = null;
        } else {
            this.extraJson = OBJECT_MAPPER.writeValueAsString(extra);
        }
    }

    @SneakyThrows
    public void setExtraJson(String extraJson) {
        this.extraJson = extraJson;
        if (Strings.isNullOrEmpty(extraJson)) {
            this.extra = Collections.emptyMap();
        } else {
            this.extra = OBJECT_MAPPER.readValue(extraJson, JacksonType.MAP_OF_STRING);
        }
    }
}
