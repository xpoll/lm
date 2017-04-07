package cn.blmdz.rabbit.settlement;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.SettlementAutoConfig;

/**
 * @author Effet
 */
@Configuration
@ComponentScan({"io.terminus.galaxy.settlement"})
@Import({SettlementAutoConfig.class})
public class GalaxySettlementConfiguration {


}
