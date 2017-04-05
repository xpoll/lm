package io.terminus.parana.user.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.parana.user.address.model.Address;
import io.terminus.parana.user.address.service.AddressReadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AddressCacher {
   private static final Logger log = LoggerFactory.getLogger(AddressCacher.class);
   private final LoadingCache namedCache;

   @Autowired
   public AddressCacher(final AddressReadService addressReadService) {
      this.namedCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public Address load(String key) throws Exception {
            Response<Address> addressRes = addressReadService.findByName(key);
            if(!addressRes.isSuccess()) {
               AddressCacher.log.error("failed to find address(name={}), error code:{}", key, addressRes.getError());
               throw new ServiceException(addressRes.getError());
            } else {
               return (Address)addressRes.getResult();
            }
         }
      });
   }

   public Response findByName(String name) {
      Response<Address> resp = new Response();

      try {
         resp.setResult(this.namedCache.get(name));
      } catch (Exception var4) {
         log.warn("failed to find address by name({}), cause: {}", name, var4.getMessage());
         resp.setResult((Object)null);
      }

      return resp;
   }
}
