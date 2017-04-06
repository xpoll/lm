package io.terminus.galaxy;

import io.terminus.parana.ItemAutoConfig;
import io.terminus.parana.item.impl.dao.ItemDao;
import io.terminus.parana.item.impl.dao.SkuDao;
import io.terminus.parana.storage.impl.manager.DefaultStorageManager;
import io.terminus.parana.storage.impl.service.DefaultStorageServiceImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * @author Effet
 */
@Configuration
@Import({ItemAutoConfig.class})
public class GalaxyItemConfiguration {

    @Bean
    public DefaultStorageManager defaultStorageManager(ItemDao itemDao, SkuDao skuDao) {
        return new DefaultStorageManager(itemDao, skuDao);
    }

    @Bean
    public DefaultStorageServiceImpl defaultStorageServiceImpl(DefaultStorageManager defaultStorageManager) {
        return new DefaultStorageServiceImpl(defaultStorageManager);
    }
}
