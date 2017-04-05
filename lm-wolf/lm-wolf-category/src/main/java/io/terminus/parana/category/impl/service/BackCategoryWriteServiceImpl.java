package io.terminus.parana.category.impl.service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.category.impl.dao.BackCategoryDao;
import io.terminus.parana.category.impl.dao.CategoryAttributeDao;
import io.terminus.parana.category.impl.manager.BackCategoryManager;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.service.BackCategoryWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BackCategoryWriteServiceImpl implements BackCategoryWriteService {
   private static final Logger log = LoggerFactory.getLogger(BackCategoryWriteServiceImpl.class);
   private final BackCategoryDao backCategoryDao;
   private final BackCategoryManager backCategoryManager;
   private final CategoryAttributeDao categoryAttributeDao;

   @Autowired
   public BackCategoryWriteServiceImpl(BackCategoryDao backCategoryDao, BackCategoryManager backCategoryManager, CategoryAttributeDao categoryAttributeDao) {
      this.backCategoryDao = backCategoryDao;
      this.backCategoryManager = backCategoryManager;
      this.categoryAttributeDao = categoryAttributeDao;
   }

   public Response create(BackCategory backCategory) {
      backCategory.setHasChildren(Boolean.valueOf(false));
      backCategory.setHasSpu(Boolean.valueOf(false));
      Integer status = (Integer)MoreObjects.firstNonNull(backCategory.getStatus(), Integer.valueOf(1));
      backCategory.setStatus(status);
      Long pid = (Long)MoreObjects.firstNonNull(backCategory.getPid(), Long.valueOf(0L));
      backCategory.setPid(pid);
      int level = ((Integer)MoreObjects.firstNonNull(backCategory.getLevel(), Integer.valueOf(1))).intValue();
      backCategory.setLevel(Integer.valueOf(level));

      try {
         BackCategory child = this.backCategoryDao.findChildrenByName(pid, backCategory.getName());
         if(Objects.equal(child, backCategory)) {
            if(Objects.equal(child.getStatus(), Integer.valueOf(1))) {
               log.error("duplicated back category name({}) of parent(id={})", backCategory.getName(), pid);
               return Response.fail("category.name.duplicated");
            } else {
               this.backCategoryDao.updateStatusById(child.getId(), Integer.valueOf(1));
               return Response.ok(child);
            }
         } else {
            if(pid.longValue() != 0L) {
               BackCategory parent = (BackCategory)this.backCategoryDao.findById(pid);
               if(parent == null) {
                  log.error("parent backCategory(id={}) not found", pid);
                  return Response.fail("category.parent.not.found");
               }

               if(parent.getHasSpu().booleanValue()) {
                  log.error("parent backCategory(id={}) has spu, failed to create child for it", pid);
                  return Response.fail("category.parent.has.spu");
               }

               Integer countOfValidAttribute = this.categoryAttributeDao.countOfValidCategoryAttribute(pid);
               if(!Objects.equal(countOfValidAttribute, Integer.valueOf(0))) {
                  log.error("parent backCategory(id={}) has attribute defined", pid);
                  return Response.fail("category.parent.has.attribute");
               }

               backCategory.setLevel(Integer.valueOf(parent.getLevel().intValue() + 1));
               this.backCategoryManager.create(backCategory, parent);
            } else {
               this.backCategoryDao.create(backCategory);
            }

            return Response.ok(backCategory);
         }
      } catch (Exception var8) {
         log.error("failed to create {}, cause:{}", backCategory, Throwables.getStackTraceAsString(var8));
         return Response.fail("category.create.fail");
      }
   }

   public Response updateName(Long categoryId, String name) {
      BackCategory backCategory = new BackCategory();
      backCategory.setId(categoryId);
      backCategory.setName(name);

      try {
         this.backCategoryDao.update(backCategory);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to update name({}) of back category(id={}), cause:{}", new Object[]{name, categoryId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("category.update.fail");
      }
   }

   public Response enable(Long categoryId) {
      try {
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("back category(id={}) not found", categoryId);
            return Response.fail("category.not.found");
         } else if(backCategory.getStatus().intValue() == 1) {
            return Response.ok(Boolean.TRUE);
         } else {
            this.backCategoryManager.enable(backCategory);
            return Response.ok();
         }
      } catch (Exception var3) {
         log.error("failed to enable back category(id={}),cause:{}", categoryId, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.update.fail");
      }
   }

   public Response disable(Long categoryId) {
      try {
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("back category(id={}) not found", categoryId);
            return Response.fail("category.not.found");
         } else if(!backCategory.getHasSpu().booleanValue() && !backCategory.getHasChildren().booleanValue()) {
            if(backCategory.getStatus().intValue() == -1) {
               return Response.ok(Boolean.TRUE);
            } else {
               this.backCategoryManager.disable(categoryId, backCategory.getPid());
               return Response.ok(Boolean.TRUE);
            }
         } else {
            log.error("back category(id={}) has children or has spu, delete failed", categoryId);
            return Response.fail("category.not.leaf");
         }
      } catch (Exception var3) {
         log.error("failed to disable back category(id={}),cause:{}", categoryId, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.update.fail");
      }
   }
}
