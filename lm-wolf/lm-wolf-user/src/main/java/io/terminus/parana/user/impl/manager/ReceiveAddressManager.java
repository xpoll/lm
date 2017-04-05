package io.terminus.parana.user.impl.manager;

import com.google.common.base.Objects;
import io.terminus.parana.user.address.model.ReceiveAddress;
import io.terminus.parana.user.impl.address.dao.ReceiveAddressDao;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class ReceiveAddressManager {
   private static final Logger log = LoggerFactory.getLogger(ReceiveAddressManager.class);
   @Autowired
   private ReceiveAddressDao receiveAddressDao;

   @Transactional
   public void makeDefault(Long addressId, Long userId, List addressList) {
      for(ReceiveAddress address : addressList) {
         if(Objects.equal(address.getIsDefault(), Boolean.valueOf(true))) {
            address.setIsDefault(Boolean.valueOf(false));
            this.receiveAddressDao.update(address);
            break;
         }
      }

      this.receiveAddressDao.makeDefault(addressId, userId);
   }
}
