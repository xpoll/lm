package cn.blmdz.rabbit;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.shop.service.GalaxyShopReadService;
import cn.blmdz.wolf.shop.model.Shop;
import cn.blmdz.wolf.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

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
