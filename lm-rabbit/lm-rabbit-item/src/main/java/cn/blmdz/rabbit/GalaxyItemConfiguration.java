package cn.blmdz.rabbit;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.ItemAutoConfig;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.impl.dao.SkuDao;
import cn.blmdz.wolf.storage.impl.manager.DefaultStorageManager;
import cn.blmdz.wolf.storage.impl.service.DefaultStorageServiceImpl;

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
