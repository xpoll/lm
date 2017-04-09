/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package cn.blmdz.rabbit.shop.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.shop.model.Shop;

/**
 * Date: 7/19/16
 * Time: 4:29 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
public interface GalaxyShopReadService {

    @Export(paramNames = "ids")
    Response<List<Shop>> findShopByIds(List<Long> ids);
}
