package io.terminus.parana.brand.impl.service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.brand.impl.dao.BrandDao;
import io.terminus.parana.brand.model.Brand;
import io.terminus.parana.brand.service.BrandWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class BrandWriteServiceImpl implements BrandWriteService {
   private static final Logger log = LoggerFactory.getLogger(BrandWriteServiceImpl.class);
   private final BrandDao brandDao;

   @Autowired
   public BrandWriteServiceImpl(BrandDao brandDao) {
      this.brandDao = brandDao;
   }

   public Response create(Brand brand) {
      String name = brand.getName();
      if(!StringUtils.hasText(name)) {
         log.error("brand name can not be empty");
         return Response.fail("brand.name.empty");
      } else {
         String normalizedName = name.trim().toLowerCase();

         try {
            Brand existed = this.brandDao.findByName(normalizedName);
            if(existed != null) {
               log.error("duplicated brand name {}", name);
               return Response.fail("brand.name.duplicated");
            } else {
               brand.setName(normalizedName);
               brand.setStatus((Integer)MoreObjects.firstNonNull(brand.getStatus(), Integer.valueOf(1)));
               this.brandDao.create(brand);
               return Response.ok(brand.getId());
            }
         } catch (Exception var5) {
            log.error("failed to create {}, cause:{}", brand, Throwables.getStackTraceAsString(var5));
            return Response.fail("brand.create.fail");
         }
      }
   }

   public Response update(Brand brand) {
      Long brandId = brand.getId();

      try {
         Brand existed = (Brand)this.brandDao.findById(brandId);
         if(existed == null) {
            log.error("brand(id={}) not exist", brandId);
            return Response.fail("brand.not.found");
         } else {
            if(StringUtils.hasText(brand.getName())) {
               brand.setName(brand.getName().trim().toLowerCase());
            }

            this.brandDao.update(brand);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var4) {
         log.error("failed to update {}, cause:{}", brand, Throwables.getStackTraceAsString(var4));
         return Response.fail("brand.update.fail");
      }
   }
}
