package io.terminus.galaxy;

import io.terminus.common.model.Response;
import io.terminus.galaxy.shop.service.GalaxyShopReadService;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Date: 7/19/16
 * Time: 4:30 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Service
public class GalaxyShopReadServiceImpl implements GalaxyShopReadService {

    private final ShopReadService shopReadService;

    @Autowired
    public GalaxyShopReadServiceImpl(ShopReadService shopReadService) {
        this.shopReadService = shopReadService;
    }

    @Override
    public Response<List<Shop>> findShopByIds(List<Long> ids) {
        return shopReadService.findByIds(ids);
    }
}
