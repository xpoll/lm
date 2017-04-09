package cn.blmdz.wolf.category.impl.service;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.FrontCategoryDao;
import cn.blmdz.wolf.category.impl.manager.CategoryBindingManager;
import cn.blmdz.wolf.category.impl.manager.FrontCategoryManager;
import cn.blmdz.wolf.category.model.FrontCategory;
import cn.blmdz.wolf.category.service.FrontCategoryWriteService;

@Service
public class FrontCategoryWriteServiceImpl implements FrontCategoryWriteService {
   private static final Logger log = LoggerFactory.getLogger(FrontCategoryWriteServiceImpl.class);
   private final FrontCategoryDao frontCategoryDao;
   private final CategoryBindingManager categoryBindingManager;
   private final FrontCategoryManager frontCategoryManager;

   @Autowired
   public FrontCategoryWriteServiceImpl(FrontCategoryDao frontCategoryDao, CategoryBindingManager categoryBindingManager, FrontCategoryManager frontCategoryManager) {
      this.frontCategoryDao = frontCategoryDao;
      this.categoryBindingManager = categoryBindingManager;
      this.frontCategoryManager = frontCategoryManager;
   }

   public Response create(FrontCategory frontCategory) {
      frontCategory.setHasChildren(Boolean.valueOf(false));
      Long pid = (Long)MoreObjects.firstNonNull(frontCategory.getPid(), Long.valueOf(0L));
      frontCategory.setPid(pid);
      int level = ((Integer)MoreObjects.firstNonNull(frontCategory.getLevel(), Integer.valueOf(1))).intValue();
      frontCategory.setLevel(Integer.valueOf(level));

      try {
         List<FrontCategory> children = this.frontCategoryDao.findChildren(pid);
         if(children.contains(frontCategory)) {
            log.error("duplicated front category name({}) of parent(id={})", frontCategory.getName(), pid);
            return Response.fail("category.name.duplicated");
         } else {
            if(pid.longValue() != 0L) {
               FrontCategory parent = (FrontCategory)this.frontCategoryDao.findById(pid);
               if(parent == null) {
                  log.error("parent front category(id={}) not found", pid);
                  return Response.fail("parent.front.category.not.found");
               }

               frontCategory.setLevel(Integer.valueOf(parent.getLevel().intValue() + 1));
               this.frontCategoryManager.create(frontCategory, parent);
            } else {
               this.frontCategoryDao.create(frontCategory);
            }

            return Response.ok(frontCategory);
         }
      } catch (Exception var6) {
         log.error("failed to create {}, cause:{}", frontCategory, Throwables.getStackTraceAsString(var6));
         return Response.fail("category.create.fail");
      }
   }

   public Response updateName(Long categoryId, String name) {
      FrontCategory frontCategory = new FrontCategory();
      frontCategory.setId(categoryId);
      frontCategory.setName(name);

      try {
         this.frontCategoryDao.update(frontCategory);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to update name({}) of front category(id={}), cause:{}", new Object[]{name, categoryId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("category.update.fail");
      }
   }

   public Response updateLogo(Long categoryId, String logo) {
      FrontCategory frontCategory = new FrontCategory();
      frontCategory.setId(categoryId);
      frontCategory.setLogo(logo);

      try {
         this.frontCategoryDao.update(frontCategory);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to update logo({}) of front category(id={}), cause:{}", new Object[]{logo, categoryId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("category.update.fail");
      }
   }

   public Response delete(Long id) {
      try {
         FrontCategory frontCategory = (FrontCategory)this.frontCategoryDao.findById(id);
         if(frontCategory == null) {
            log.error("front category(id={}) not found", id);
            return Response.fail("category.not.found");
         } else if(frontCategory.getHasChildren().booleanValue()) {
            log.error("front category(id={}) still has children, it can not be deleted", id);
            return Response.fail("category.has.children");
         } else {
            this.categoryBindingManager.deleteFrontCategory(frontCategory);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var3) {
         log.error("failed to delete front category(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.delete.fail");
      }
   }
}
