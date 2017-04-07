package cn.blmdz.rabbit.admin.event;

import java.util.List;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.api.client.util.Lists;
import com.google.common.base.Optional;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.model.Seller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import cn.blmdz.rabbit.user.service.SellerWriteService;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.item.service.ItemWriteService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.web.core.events.shop.ShopFrozenEvent;
import cn.blmdz.wolf.web.core.events.shop.ShopUnfrozenEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 6/30/16
 * Time: 11:00 AM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Component
@SuppressWarnings("unused")
public class ShopEventListener {
    private final EventBus eventBus;
    private final ShopReadService shopReadService;
    private final ItemReadService itemReadService;
    private final ItemWriteService itemWriteService;
    private final SellerReadService sellerReadService;
    private final SellerWriteService sellerWriteService;

    @Autowired
    public ShopEventListener(EventBus eventBus,
                             ShopReadService shopReadService,
                             ItemReadService itemReadService,
                             ItemWriteService itemWriteService,
                             SellerReadService sellerReadService,
                             SellerWriteService sellerWriteService) {
        this.eventBus = eventBus;
        this.shopReadService = shopReadService;
        this.itemReadService = itemReadService;
        this.itemWriteService = itemWriteService;
        this.sellerReadService = sellerReadService;
        this.sellerWriteService = sellerWriteService;
    }

    @PostConstruct
    public void init() {
        eventBus.register(this);
    }

    @Subscribe
    public void onShopFrozen(ShopFrozenEvent event) {
       doUpdateSellerAndItem(event.getShopId(), 1, -2);
    }

    @Subscribe
    public void onShopunFrozen(ShopUnfrozenEvent event) {
        doUpdateSellerAndItem(event.getShopId(), -2, -1);
    }

    private void doUpdateSellerAndItem(Long shopId, Integer fromItemStatus, Integer status) {
        // 冻结卖家
        Response<Shop> findShop = shopReadService.findById(shopId);
        if (!findShop.isSuccess()) {
            log.error("fail to find shop by id: {}, cause:{}", shopId, findShop.getError());
            return;
        }
        final Shop shop = findShop.getResult();

        Response<Optional<Seller>> findSeller = sellerReadService.findSellerByUserId(shop.getUserId());
        if (!(findSeller.isSuccess() && findSeller.getResult().isPresent())) {
            log.error("fail to find seller by user id:{}, cause:{}", shop.getUserId(), findSeller.getError());
            return;
        }
        final Seller seller = findSeller.getResult().get();

        Seller toUpdate = new Seller();
        toUpdate.setId(seller.getId());
        toUpdate.setStatus(status);
        Response<Boolean> tryUpdate = sellerWriteService.updateSeller(toUpdate);
        if (!tryUpdate.isSuccess()) {
            log.error("fail to frozen seller:{}, cause:{}", toUpdate, tryUpdate.getResult());
            return;
        }

        // 冻结商品
        // 获取第一页
        ParanaUser criteria = new ParanaUser();
        criteria.setId(shop.getUserId());
        criteria.setShopId(shopId);
        int page = 1;
        Response<Paging<Item>> findItem = itemReadService.findBy(criteria, null, null, null, fromItemStatus, page, 200);
        if (!findItem.isSuccess()) {
            log.error("fail to find item by seller:{}, cause:{}", seller, findItem.getResult());
        }
        Paging<Item> itemPage = findItem.getResult();

        while (itemPage.getTotal() != 0L && itemPage.getData().size() > 0) {
            List<Long> ids = Lists.newArrayList();
            for (Item item : itemPage.getData()) {
                ids.add(item.getId());
            }
            itemWriteService.batchUpdateStatusByShopIdAndItemIds(shop.getId(), ids, status);

            // 下一页
            page += 1;
            findItem = itemReadService.findBy(criteria, null, null, null, 1, page, 200);
            itemPage = findItem.getResult();
        }
    }
}
