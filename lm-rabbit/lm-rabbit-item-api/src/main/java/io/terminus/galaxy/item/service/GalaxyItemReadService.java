/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package io.terminus.galaxy.item.service;

import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.item.dto.SearchItemDto;
import io.terminus.pampas.client.Export;
import io.terminus.parana.item.model.Item;

import java.util.List;

/**
 * Date: 7/19/16
 * Time: 1:03 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
public interface GalaxyItemReadService {
    @Export("shopId")
    Response<Long> countItemByShop(Long shopId);

    @Export("itemId")
    Response<Item> findItemByIdWithCache(Long itemId);

    @Export(paramNames = "itemIds")
    Response<Paging<SearchItemDto>> findSearchItemDtoByIds(String itemIds);
}
