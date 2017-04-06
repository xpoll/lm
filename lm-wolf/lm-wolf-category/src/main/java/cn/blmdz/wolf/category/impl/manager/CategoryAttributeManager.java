package cn.blmdz.wolf.category.impl.manager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.category.impl.dao.CategoryAttributeDao;
import cn.blmdz.wolf.parana.category.dto.ExchangeIndexDto;
import cn.blmdz.wolf.parana.category.model.CategoryAttribute;

@Component
public class CategoryAttributeManager {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributeManager.class);
   private final CategoryAttributeDao categoryAttributeDao;

   @Autowired
   public CategoryAttributeManager(CategoryAttributeDao categoryAttributeDao) {
      this.categoryAttributeDao = categoryAttributeDao;
   }

   @Transactional(
      isolation = Isolation.REPEATABLE_READ
   )
   public void exchange(ExchangeIndexDto exchangeIndexDto) {
      Long id1 = exchangeIndexDto.getAttributeId1();
      CategoryAttribute a1 = this.findForExchange(id1);
      Long id2 = exchangeIndexDto.getAttributeId2();
      CategoryAttribute a2 = this.findForExchange(id2);
      this.categoryAttributeDao.updateIndex(id1, a2.getIndex());
      this.categoryAttributeDao.updateIndex(id2, a1.getIndex());
   }

   private CategoryAttribute findForExchange(Long id) {
      CategoryAttribute attr = (CategoryAttribute)this.categoryAttributeDao.findById(id);
      if(attr == null) {
         log.warn("category attribute not found for exchange, id={}", id);
         throw new ServiceException("category.attribute.not.exist");
      } else {
         return attr;
      }
   }
}
