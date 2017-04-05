package io.terminus.parana.pay.mock.cacher;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import java.util.concurrent.TimeUnit;

public class WapalipayCache {
   public static Cache wapalipayCache = CacheBuilder.newBuilder().maximumSize(1000L).expireAfterWrite(10L, TimeUnit.MINUTES).build();
}
