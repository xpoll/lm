/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package cn.blmdz.rabbit.item.service;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.item.dto.SearchItemDto;
import cn.blmdz.wolf.item.model.Item;

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
