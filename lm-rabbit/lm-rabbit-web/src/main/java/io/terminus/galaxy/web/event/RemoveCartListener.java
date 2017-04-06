package io.terminus.galaxy.web.event;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import io.terminus.common.model.Response;
import io.terminus.parana.cart.service.CartWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.PostConstruct;

/**
 * Mail: F@terminus.io
 * Data: 16/4/14
 * Author: yangzefeng
 */
@Component @Slf4j
public class RemoveCartListener {

    @Autowired
    private CartWriteService cartWriteService;

    @Autowired
    private EventBus eventBus;

    @PostConstruct
    public void register() {
        eventBus.register(this);
    }

    @Subscribe
    public void removeCart(RemoveCartEvent event) {
        if (CollectionUtils.isEmpty(event.getSkuIds())) {
            return;
        }
        Response<Boolean> removeR = cartWriteService.batchDelete(event.getSkuIds(),
                event.getUserId());
        if (!removeR.isSuccess()) {
            log.error("fail to remove cart by event {}, error code:{}",
                    event, removeR.getError());
        }
    }
}
