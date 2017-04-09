package cn.blmdz.rabbit.settlement;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import cn.blmdz.wolf.SettlementAutoConfig;

/**
 * @author Effet
 */
@Configuration
@ComponentScan({"cn.blmdz.rabbit.settlement"})
@Import({SettlementAutoConfig.class})
public class GalaxySettlementConfiguration {


}
