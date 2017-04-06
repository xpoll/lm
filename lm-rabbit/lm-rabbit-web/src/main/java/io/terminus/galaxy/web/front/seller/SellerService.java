package io.terminus.galaxy.web.front.seller;

import com.google.common.base.Optional;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.pampas.client.Export;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Effet
 */
@Slf4j
@Service
public class SellerService {

    private final SellerReadService sellerReadService;

    @Autowired
    public SellerService(SellerReadService sellerReadService) {
        this.sellerReadService = sellerReadService;
    }

    @Export(paramNames = {"user"})
    public Response<Seller> findSellerByLoginUser(BaseUser user) {
        return orEmpty(sellerReadService.findSellerByUserId(user.getId()));
    }

    private Response<Seller> orEmpty(Response<Optional<Seller>> resp) {
        if (resp.isSuccess()) {
            return Response.ok(resp.getResult().or(new Seller()));
        }
        return Response.fail(resp.getError());
    }
}
