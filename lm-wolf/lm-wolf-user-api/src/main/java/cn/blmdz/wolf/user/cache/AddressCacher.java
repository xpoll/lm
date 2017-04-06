package cn.blmdz.wolf.user.cache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.address.model.Address;
import cn.blmdz.wolf.user.address.service.AddressReadService;

@Component
public class AddressCacher {
   private static final Logger log = LoggerFactory.getLogger(AddressCacher.class);
   private final LoadingCache<String, Address> namedCache;

   @Autowired
   public AddressCacher(final AddressReadService addressReadService) {
      this.namedCache = CacheBuilder.newBuilder().build(new CacheLoader<String, Address>() {
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
         resp.setResult(null);
      }

      return resp;
   }
}
