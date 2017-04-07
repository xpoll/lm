package cn.blmdz.rabbit.web.core.trade.event;

import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.storage.service.StorageService;
import lombok.extern.slf4j.Slf4j;

/**
 * Desc:
 * Mail: F@terminus.io
 * Data: 16/6/28
 * Author: yangzefeng
 */
@Component @Slf4j
public class RollbackStockListener {

    @Autowired
    private StorageService storageService;

    @Autowired
    private EventBus eventBus;

    @PostConstruct
    public void register() {
        eventBus.register(this);
    }

    @Subscribe
    public void stockChange(RollbakStockEvent event) {
        Map<Long, Integer> skuIdAndQuantity = event.getSkuIdAndQuantity();
        if (CollectionUtils.isEmpty(skuIdAndQuantity)) {
            return;
        }
        for (Map.Entry<Long, Integer> entry : skuIdAndQuantity.entrySet()) {
            Response<Boolean> changeR = storageService.increaseBy(
                    entry.getKey(), null, null, entry.getValue()
            );
            if (!changeR.isSuccess()) {
                log.error("fail to change stock by sku id {}, quantity {}, error code:{}",
                        entry.getKey(), entry.getValue(), changeR.getError());
            }
        }
    }
}
