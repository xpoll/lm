package cn.blmdz.wolf.category.impl.service.attribute;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.category.impl.dao.BackCategoryDao;
import cn.blmdz.wolf.category.impl.dao.CategoryAttributeDao;
import cn.blmdz.wolf.category.impl.manager.CategoryAttributeManager;
import cn.blmdz.wolf.parana.category.dto.ExchangeIndexDto;
import cn.blmdz.wolf.parana.category.model.BackCategory;
import cn.blmdz.wolf.parana.category.model.CategoryAttribute;
import cn.blmdz.wolf.parana.category.service.CategoryAttributeWriteService;

@Service
public class CategoryAttributeWriteServiceImpl implements CategoryAttributeWriteService {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributeWriteServiceImpl.class);
   private final BackCategoryDao backCategoryDao;
   private final CategoryAttributeDao categoryAttributeDao;
   private final CategoryAttributeManager categoryAttributeManager;

   @Autowired
   public CategoryAttributeWriteServiceImpl(BackCategoryDao backCategoryDao, CategoryAttributeDao categoryAttributeDao, CategoryAttributeManager categoryAttributeManager) {
      this.backCategoryDao = backCategoryDao;
      this.categoryAttributeDao = categoryAttributeDao;
      this.categoryAttributeManager = categoryAttributeManager;
   }

   public Response create(CategoryAttribute categoryAttribute) {
      try {
         categoryAttribute.setStatus(Integer.valueOf(1));
         Long categoryId = categoryAttribute.getCategoryId();
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("back category(id={}) not found", categoryId);
            return Response.fail("category.not.found");
         } else if(backCategory.getHasChildren().booleanValue()) {
            log.error("back category(id={}) has children, no attribute allowed", categoryId);
            return Response.fail("category.has.children");
         } else {
            String attrKey = categoryAttribute.getAttrKey();
            if(!StringUtils.isEmpty(attrKey) && !attrKey.contains(":") && !attrKey.contains("_")) {
               CategoryAttribute existed = this.categoryAttributeDao.findByCategoryIdAndAttrKey(categoryId, attrKey);
               if(existed != null && existed.getStatus().intValue() == 1) {
                  log.error("duplicated attrKey({}) under category(id={})", attrKey, categoryId);
                  return Response.fail("attrKey.duplicated");
               } else {
                  Integer index = Integer.valueOf(this.categoryAttributeDao.maxIndexOfCategoryId(categoryId).intValue() + 1);
                  categoryAttribute.setIndex(index);
                  if(existed != null) {
                     BeanMapper.copy(categoryAttribute, existed);
                     this.categoryAttributeDao.update(existed);
                     return Response.ok(existed.getId());
                  } else {
                     this.categoryAttributeDao.create(categoryAttribute);
                     return Response.ok(categoryAttribute.getId());
                  }
               }
            } else {
               log.error("invalid attrKey:{}", attrKey);
               return Response.fail("attrKey.invalid");
            }
         }
      } catch (Exception var7) {
         log.error("failed to create {}, cause:{}", categoryAttribute, Throwables.getStackTraceAsString(var7));
         return Response.fail("category.attribute.create.fail");
      }
   }

   public Response update(CategoryAttribute categoryAttribute) {
      Long id = categoryAttribute.getId();

      try {
         CategoryAttribute existed = (CategoryAttribute)this.categoryAttributeDao.findById(id);
         if(existed == null) {
            log.error("category attribute(id={}) not found", id);
            return Response.fail("category.attribute.not.found");
         } else {
            this.categoryAttributeDao.update(categoryAttribute);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var4) {
         log.error("failed to update {}, cause:{}", categoryAttribute, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.attribute.update.fail");
      }
   }

   public Response delete(Long id) {
      try {
         this.categoryAttributeDao.delete(id);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to delete category attribute(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.attribute.delete.fail");
      }
   }

   public Response exchangeIndex(ExchangeIndexDto exchangeIndexDto) {
      try {
         this.categoryAttributeManager.exchange(exchangeIndexDto);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to exchange attribute {}, cause:{}", exchangeIndexDto, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.attribute.exchange.fail");
      }
   }
}
