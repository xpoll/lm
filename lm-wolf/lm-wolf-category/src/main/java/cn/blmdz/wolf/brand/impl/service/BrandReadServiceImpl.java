package cn.blmdz.wolf.brand.impl.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.brand.impl.dao.BrandDao;
import cn.blmdz.wolf.parana.brand.model.Brand;
import cn.blmdz.wolf.parana.brand.service.BrandReadService;

@Service
public class BrandReadServiceImpl implements BrandReadService {
   private static final Logger log = LoggerFactory.getLogger(BrandReadServiceImpl.class);
   private final BrandDao brandDao;

   @Autowired
   public BrandReadServiceImpl(BrandDao brandDao) {
      this.brandDao = brandDao;
   }

   public Response findById(Long brandId) {
      try {
         Brand brand = (Brand)this.brandDao.findById(brandId);
         if(brand == null) {
            log.error("brand(id={}) not found", brandId);
            return Response.fail("brand.not.found");
         } else {
            return Response.ok(brand);
         }
      } catch (Exception var3) {
         log.error("failed to find brand by id={}, cause:{}", brandId, Throwables.getStackTraceAsString(var3));
         return Response.fail("brand.find.fail");
      }
   }

   public Response findByIds(List brandIds) {
      try {
         List<Brand> brands = this.brandDao.findByIds(brandIds);
         return Response.ok(brands);
      } catch (Exception var3) {
         log.error("failed to find brands by ids({}),cause:{}", brandIds, Throwables.getStackTraceAsString(var3));
         return Response.fail("brand.find.fail");
      }
   }

   public Response findByName(String name) {
      if(!StringUtils.hasText(name)) {
         log.error("empty brand name");
         return Response.fail("brand.find.fail");
      } else {
         try {
            Brand brand = this.brandDao.findByName(name.trim().toLowerCase());
            if(brand == null) {
               log.error("brand(name={}) not found", name);
               return Response.fail("brand.not.found");
            } else {
               return Response.ok(brand);
            }
         } catch (Exception var3) {
            log.error("failed to find brand by name={}, cause:{}", name, Throwables.getStackTraceAsString(var3));
            return Response.fail("brand.find.fail");
         }
      }
   }

   public Response exist(String name) {
      if(!StringUtils.hasText(name)) {
         log.warn("empty brand name");
         return Response.ok(Boolean.FALSE);
      } else {
         try {
            Brand brand = this.brandDao.findByName(name.trim().toLowerCase());
            if(brand == null) {
               log.error("brand(name={}) not found", name);
               return Response.ok(Boolean.FALSE);
            } else {
               return Response.ok(Boolean.TRUE);
            }
         } catch (Exception var3) {
            log.error("failed to find brand by name={}, cause:{}", name, Throwables.getStackTraceAsString(var3));
            return Response.fail("brand.find.fail");
         }
      }
   }

   public Response findByNamePrefix(String namePrefix, Integer count) {
      if(!StringUtils.hasText(namePrefix)) {
         log.warn("empty brand name");
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<Brand> brands = this.brandDao.findByNamePrefix(namePrefix.trim().toLowerCase(), count);
            return Response.ok(brands);
         } catch (Exception var4) {
            log.error("failed to find brands by namePrefix={}, cause:{}", namePrefix, Throwables.getStackTraceAsString(var4));
            return Response.fail("brand.find.fail");
         }
      }
   }

   public Response pagination(Integer pageNo, Integer pageSize, String namePrefix) {
      try {
         PageInfo page = new PageInfo(pageNo, pageSize);
         Map<String, Object> params = Maps.newHashMap();
         if(!Strings.isNullOrEmpty(namePrefix)) {
            params.put("namePrefix", namePrefix);
         }

         return Response.ok(this.brandDao.paging(page.getOffset(), page.getLimit(), params));
      } catch (Exception var6) {
         log.error("failed to paging brands(pageNo={}, pageSize={}, name={}), cause: {}", new Object[]{pageNo, pageSize, namePrefix, Throwables.getStackTraceAsString(var6)});
         return Response.fail("brand.find.fail");
      }
   }
}
