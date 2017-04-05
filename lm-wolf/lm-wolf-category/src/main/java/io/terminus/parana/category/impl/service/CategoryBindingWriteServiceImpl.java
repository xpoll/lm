package io.terminus.parana.category.impl.service;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.parana.category.impl.dao.CategoryBindingDao;
import io.terminus.parana.category.impl.manager.CategoryBindingManager;
import io.terminus.parana.category.model.CategoryBinding;
import io.terminus.parana.category.service.CategoryBindingWriteService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class CategoryBindingWriteServiceImpl implements CategoryBindingWriteService {
   private static final Logger log = LoggerFactory.getLogger(CategoryBindingWriteServiceImpl.class);
   private final CategoryBindingDao categoryBindingDao;
   private final CategoryBindingManager categoryBindingManager;

   @Autowired
   public CategoryBindingWriteServiceImpl(CategoryBindingDao categoryBindingDao, CategoryBindingManager categoryBindingManager) {
      this.categoryBindingDao = categoryBindingDao;
      this.categoryBindingManager = categoryBindingManager;
   }

   public Response bind(Long frontCategoryId, Long backCategoryId) {
      try {
         CategoryBinding existedBinding = this.categoryBindingDao.findByFrontBackCategoryId(frontCategoryId, backCategoryId);
         if(existedBinding != null) {
            log.error("front category(id={}) has bond back category(id={}) already", frontCategoryId, backCategoryId);
            return Response.fail("duplicated.binding");
         } else {
            CategoryBinding categoryBinding = new CategoryBinding();
            categoryBinding.setFrontCategoryId(frontCategoryId);
            categoryBinding.setBackCategoryId(backCategoryId);
            this.categoryBindingDao.create(categoryBinding);
            return Response.ok(categoryBinding.getId());
         }
      } catch (Exception var5) {
         log.error("failed to bind front category(id={}) to back category(id={}), cause:{}", new Object[]{frontCategoryId, backCategoryId, Throwables.getStackTraceAsString(var5)});
         return Response.fail("category.bind.fail");
      }
   }

   public Response multiBind(Long frontCategoryId, List backCategoryIds) {
      if(CollectionUtils.isEmpty(backCategoryIds)) {
         log.error("empty back category binding for front category(id={})", frontCategoryId);
         return Response.fail("category.bind.fail");
      } else {
         try {
            List<CategoryBinding> categoryBindings = Lists.newArrayListWithCapacity(backCategoryIds.size());

            for(Long backCategoryId : backCategoryIds) {
               CategoryBinding categoryBinding = new CategoryBinding();
               categoryBinding.setFrontCategoryId(frontCategoryId);
               categoryBinding.setBackCategoryId(backCategoryId);
               categoryBindings.add(categoryBinding);
            }

            this.categoryBindingManager.multiBind(categoryBindings);
            return Response.ok(Boolean.TRUE);
         } catch (Exception var7) {
            log.error("failed to bind back category for front category(id={}), cause:{}", frontCategoryId, Throwables.getStackTraceAsString(var7));
            return Response.fail("category.bind.fail");
         }
      }
   }

   public Response unBind(Long frontCategoryId, Long backCategoryId) {
      try {
         CategoryBinding categoryBinding = this.categoryBindingDao.findByFrontBackCategoryId(frontCategoryId, backCategoryId);
         if(categoryBinding == null) {
            return Response.ok(Boolean.TRUE);
         } else {
            this.categoryBindingDao.delete(categoryBinding.getId());
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var4) {
         log.error("failed to delete binding(frontCategoryId={}, backCategoryId={}),cause:{}", new Object[]{frontCategoryId, backCategoryId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("category.unbind.fail");
      }
   }

   public Response multiUnBind(Long frontCategoryId, List backCategoryIds) {
      throw new UnsupportedOperationException("not implement yet");
   }
}
