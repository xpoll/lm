/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package io.terminus.galaxy.shop.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.shop.model.Shop;

import java.util.List;

/**
 * Date: 7/19/16
 * Time: 4:29 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
public interface GalaxyShopReadService {

    @Export(paramNames = "ids")
    Response<List<Shop>> findShopByIds(List<Long> ids);
}
