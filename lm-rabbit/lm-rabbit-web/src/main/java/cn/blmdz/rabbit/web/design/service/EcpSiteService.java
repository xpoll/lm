/*
 * <!--
 *   ~ Copyright (c) 2014 杭州端点网络科技有限公司
 *   -->
 */

package cn.blmdz.rabbit.web.design.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.web.design.modal.EcpAppKey;
import cn.blmdz.rabbit.web.design.modal.ShopSite;

/**
 * Created by yangzefeng on 14/11/12
 */
public interface EcpSiteService {
    /**
     * 返回一个用户在某个模式下的店铺站点id，当不存在时会创建，永远不会返回 null
     */
    Long getCurrentShopSiteIdByApp(Long userId, EcpAppKey appKey);

    void setCurrentShopSiteByMode(Long userId, Site site);

    @Export(paramNames = {"currentUser"})
    Map<String, List<ShopSite>> listShopSites(BaseUser currentUser);

    @Export
    Map<String, Object> listSitesWithLayouts();

    void initShopSite(Long userId);

    void createShopSite(Long userId, EcpAppKey appKey, String layoutKey);

    boolean isShopLayout(Site site);
}
