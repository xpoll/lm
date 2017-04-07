package cn.blmdz.rabbit.user;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.article.impl.ArticleAutoConfig;
import cn.blmdz.wolf.file.FileAutoConfig;
import cn.blmdz.wolf.user.ExtraUserAutoConfig;
import cn.blmdz.wolf.user.UserAutoConfig;

/**
 * @author Effet
 */
@Configuration
@ComponentScan(basePackages = {
        "io.terminus.galaxy.user.impl"
})
@Import({UserAutoConfig.class, ExtraUserAutoConfig.class, FileAutoConfig.class, ArticleAutoConfig.class})
public class GalaxyUserConfiguration {
}
