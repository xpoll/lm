package io.terminus.galaxy.settlement;

import io.terminus.parana.SettlementAutoConfig;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;

/**
 * @author Effet
 */
@Configuration
@ComponentScan({"io.terminus.galaxy.settlement"})
@Import({SettlementAutoConfig.class})
public class GalaxySettlementConfiguration {


}
