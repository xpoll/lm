package io.terminus.parana.user.impl.address.service;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.user.address.model.Address;
import io.terminus.parana.user.address.service.AddressReadService;
import io.terminus.parana.user.impl.address.dao.AddressDao;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AddressReadServiceImpl implements AddressReadService {
   private static final Logger log = LoggerFactory.getLogger(AddressReadServiceImpl.class);
   private final LoadingCache childrenCache;
   private final LoadingCache idCache;
   private static final Integer MAX_ADDRESS_LEVEL = Integer.valueOf(4);
   private final AddressDao addressDao;

   @Autowired
   public AddressReadServiceImpl(AddressDao dao) {
      this.addressDao = dao;
      this.childrenCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public List load(Integer id) throws Exception {
            return AddressReadServiceImpl.this.addressDao.findByPid(id);
         }
      });
      this.idCache = CacheBuilder.newBuilder().build(new CacheLoader() {
         public Address load(Integer id) throws Exception {
            return AddressReadServiceImpl.this.addressDao.findById(id);
         }
      });
   }

   public Response provinces() {
      Response<List<Address>> resp = new Response();

      try {
         resp.setResult(this.listProvinces());
      } catch (Exception var3) {
         log.error("failed to find all provinces, cause: {}", Throwables.getStackTraceAsString(var3));
         resp.setError("address.provinces.fail");
      }

      return resp;
   }

   public Response findById(Integer id) {
      Response<Address> resp = new Response();

      try {
         resp.setResult(this.idCache.get(id));
      } catch (Exception var4) {
         log.error("failed to find address by id({}), cause: {}", id, Throwables.getStackTraceAsString(var4));
         resp.setError("address.find.fail");
      }

      return resp;
   }

   public Response citiesOf(Integer provinceId) {
      Response<List<Address>> resp = new Response();

      try {
         resp.setResult(this.childrenCache.getUnchecked(provinceId));
      } catch (Exception var4) {
         log.error("failed to find province(id={})\'s cities, cause: {}", provinceId, Throwables.getStackTraceAsString(var4));
         resp.setError("address.find.fail");
      }

      return resp;
   }

   public Response regionsOf(Integer cityId) {
      Response<List<Address>> resp = new Response();

      try {
         resp.setResult(this.childrenCache.getUnchecked(cityId));
      } catch (Exception var4) {
         log.error("failed to find city(id={})\'s regions, cause: {}", cityId, Throwables.getStackTraceAsString(var4));
         resp.setError("address.regions.find.fail");
      }

      return resp;
   }

   public Response streetsOf(Integer regionId) {
      Response<List<Address>> resp = new Response();

      try {
         resp.setResult(this.childrenCache.getUnchecked(regionId));
      } catch (Exception var4) {
         log.error("failed to find region(id={})\'s streets, cause: {}", regionId, Throwables.getStackTraceAsString(var4));
         resp.setError("address.regions.find.fail");
      }

      return resp;
   }

   public Response childAddressOf(Integer addressId) {
      Response<List<Address>> result = new Response();

      try {
         if(Arguments.equalWith(addressId, Integer.valueOf(0))) {
            result.setResult(this.listProvinces());
         } else {
            result.setResult(this.childrenCache.getUnchecked(addressId));
         }
      } catch (Exception var4) {
         log.error("fail to find child address by id {}, cause:{}", addressId, Throwables.getStackTraceAsString(var4));
         result.setError("address.query.fail");
      }

      return result;
   }

   public Response ancestorsOf(Integer addressId) {
      Response<List<Integer>> resp = new Response();

      try {
         List<Address> addresses = Lists.newArrayListWithExpectedSize(MAX_ADDRESS_LEVEL.intValue());

         Integer id;
         Address address;
         for(id = addressId; id.intValue() > 1; id = address.getPid()) {
            address = (Address)this.idCache.getUnchecked(id);
            addresses.add(address);
         }

         address = Lists.newArrayListWithCapacity(MAX_ADDRESS_LEVEL.intValue());

         for(Address address : addresses) {
            address.add(address.getId());
         }

         address.add(id);
         resp.setResult(address);
      } catch (Exception var8) {
         log.error("failed to find ancestors of address(id={}), cause: {}", addressId, Throwables.getStackTraceAsString(var8));
         resp.setError("address.ancestors.find.fail");
      }

      return resp;
   }

   public Response ancestorOfAddresses(Integer addressId) {
      Response<List<Address>> resp = new Response();

      try {
         List<Address> addresses = Lists.newArrayListWithExpectedSize(MAX_ADDRESS_LEVEL.intValue());

         Address address;
         for(Integer id = addressId; id.intValue() > 1; id = address.getPid()) {
            address = (Address)this.idCache.getUnchecked(id);
            addresses.add(address);
         }

         resp.setResult(addresses);
      } catch (Exception var6) {
         log.error("failed to find address(id={})\'s ancestors, cause: {}", addressId, Throwables.getStackTraceAsString(var6));
         resp.setError("address.ancestors.find.fail");
      }

      return resp;
   }

   public Response getTreeOf(Integer pid) {
      Response<List<Address>> resp = new Response();

      try {
         List<Address> children = this.recursiveGetLeaves((List)this.childrenCache.getUnchecked(pid));
         resp.setResult(children);
      } catch (Exception var4) {
         log.error("failed to find address(pid={})\'s tree, cause: {}", pid, Throwables.getStackTraceAsString(var4));
         resp.setError("address.tree.failed");
      }

      return resp;
   }

   public Response findByName(String name) {
      Response<Address> resp = new Response();

      try {
         resp.setResult(this.addressDao.findByName(name));
      } catch (Exception var4) {
         log.warn("failed to find address by name({}), cause: {}", name, var4.getMessage());
         resp.setResult((Object)null);
      }

      return resp;
   }

   private List listProvinces() {
      return (List)this.childrenCache.getUnchecked(Integer.valueOf(1));
   }

   private List recursiveGetLeaves(List tree) {
      List<Address> leaves = Lists.newArrayList();
      leaves.addAll(tree);

      for(Address address : tree) {
         if(address.getLevel().intValue() != 4) {
            leaves.addAll(this.recursiveGetLeaves((List)this.childrenCache.getUnchecked(address.getId())));
         }
      }

      return leaves;
   }
}
