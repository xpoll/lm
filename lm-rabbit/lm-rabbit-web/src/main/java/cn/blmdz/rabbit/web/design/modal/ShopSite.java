package cn.blmdz.rabbit.web.design.modal;

import cn.blmdz.hunt.engine.config.model.Render;
import lombok.Getter;
import lombok.Setter;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/13
 */
public class ShopSite {
    @Getter
    @Setter
    private Long siteId;
    @Getter
    @Setter
    private boolean active;
    @Getter
    @Setter
    private String layoutKey;
    @Getter
    @Setter
    private Render.Layout layout;
}
