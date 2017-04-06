package io.terminus.galaxy.web.event;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import io.terminus.common.model.Response;
import io.terminus.parana.storage.service.StorageService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.PostConstruct;
import java.util.Map;

/**
 * Mail: F@terminus.io
 * Data: 16/4/14
 * Author: yangzefeng
 */
@Component @Slf4j
public class StockChangeListerner {

    @Autowired
    private StorageService storageService;

    @Autowired
    private EventBus eventBus;

    @PostConstruct
    public void register() {
        eventBus.register(this);
    }

    @Subscribe
    public void stockChange(StockChangeEvent event) {
        Map<Long, Integer> skuIdAndQuantity = event.getSkuIdAndQuantity();
        if (CollectionUtils.isEmpty(skuIdAndQuantity)) {
            return;
        }
        for (Map.Entry<Long, Integer> entry : skuIdAndQuantity.entrySet()) {
            Response<Boolean> changeR = storageService.decreaseBy(
                    entry.getKey(), null, null, entry.getValue()
            );
            if (!changeR.isSuccess()) {
                log.error("fail to change stock by sku id {}, quantity {}, error code:{}",
                        entry.getKey(), entry.getValue(), changeR.getError());
            }
        }
    }
}
